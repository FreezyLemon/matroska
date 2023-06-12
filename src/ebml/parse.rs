use crc::{Algorithm, Crc};
use nom::{
    bytes::streaming::take,
    combinator::{map, map_res, opt},
    number::streaming::u8,
    sequence::{preceded, tuple},
    Err::Incomplete,
    Needed, Parser,
};
use uuid::Uuid;

use super::error::{ebml_err, Error, ErrorKind};

pub type EbmlResult<'a, T> = nom::IResult<&'a [u8], T, Error>;

pub trait EbmlParsable<'a, const ID: u32>: Sized {
    /// Whether to check for a CRC-32 Element and validate the checksum.
    fn has_crc() -> bool {
        false
    }

    fn parse(input: &'a [u8], default: Option<Self>) -> EbmlResult<Self>;
}

impl<'a, const ID: u32> EbmlParsable<'a, ID> for i64 {
    fn parse(input: &'a [u8], default: Option<Self>) -> EbmlResult<Self> {
        let (i, sz) = preceded(check_id(ID), elem_size)(input)?;
        
        match sz {
            0 => Ok((i, default.unwrap_or(0))),
            l @ 0..=8 => map(take(l), int)(input),
            _ => ebml_err(ID, ErrorKind::IntTooWide)
        }
    }
}

impl<'a, const ID: u32> EbmlParsable<'a, ID> for u64 {
    fn parse(input: &'a [u8], default: Option<Self>) -> EbmlResult<Self> {
        let (i, sz) = preceded(check_id(ID), elem_size)(input)?;

        match sz {
            0 => Ok((i, default.unwrap_or(0))),
            l @ 0..=8 => map(take(l), uint)(input),
            _ => ebml_err(ID, ErrorKind::UintTooWide)
        }
    }
}

impl<'a, const ID: u32> EbmlParsable<'a, ID> for u32 {
    fn parse(input: &'a [u8], default: Option<Self>) -> EbmlResult<Self> {
        let (i, sz) = preceded(check_id(ID), elem_size)(input)?;

        match sz {
            0 => Ok((i, default.unwrap_or(0))),
            l @ 0..=4 => map(take(l), uint_short)(input),
            _ => ebml_err(ID, ErrorKind::UintTooWide)
        }
    }
}

// FIXME: Define and double-check float parsing behaviour in error cases
// FIXME: Also implement a test suite for that
impl<'a, const ID: u32> EbmlParsable<'a, ID> for f64 {
    fn parse(input: &'a [u8], default: Option<Self>) -> EbmlResult<Self> {
        let (i, sz) = preceded(check_id(ID), elem_size)(input)?;

        match sz {
            0 => Ok((i, default.unwrap_or(0.0))),
            4 => map(map(take(4), f32::from_be_bytes), f64::from)(input),
            8 => map(take(8), f64::from_be_bytes)(input),
            _ => ebml_err(ID, ErrorKind::FloatWidthIncorrect),
        }
    }
}

/// Date Element. Contains the number of nanoseconds since
/// 2001-01-01T00:00:00.000000000 UTC.
///
/// This struct can't really do anything by itself. If you want
/// date/time handling, you should use a crate like [time].
///
/// [time]: https://crates.io/crates/time
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Date(pub i64);

impl<'a, const ID: u32> EbmlParsable<'a, ID> for Date {
    fn parse(input: &'a [u8], default: Option<Self>) -> EbmlResult<Self> {
        let (i, sz) = preceded(check_id(ID), elem_size)(input)?;

        match sz {
            0 => Ok((i, default.unwrap_or(Date(0)))),
            8 => map(map(take(8), int), Date)(input),
            _ => ebml_err(ID, ErrorKind::DateWidthIncorrect),
        }
    }
}

impl<'a, const ID: u32> EbmlParsable<'a, ID> for String {
    fn parse(input: &'a [u8], default: Option<Self>) -> EbmlResult<Self> {
        map_res(get_elem_data::<ID>, |data| String::from_utf8(data).map_err(|_| ErrorKind::StringNotUtf8))(input)
    }
}

impl<'a, const ID: u32, const N: usize> EbmlParsable<'a, ID> for [u8; N] {
    fn parse(input: &'a [u8], default: Option<Self>) -> EbmlResult<Self> {
        map(get_elem_data::<ID>, |d| d.try_into().map_err(|_| ErrorKind::BinaryWidthIncorrect(d.len() as u16)))(input)
    }
}

impl<'a, const ID: u32> EbmlParsable<'a, ID> for Vec<u8> {
    fn parse(input: &'a [u8], default: Option<Self>) -> EbmlResult<Self> {
        get_elem_data::<ID>(input)
    }
}

impl<'a, const ID: u32> EbmlParsable<'a, ID> for &'a [u8] {
    fn parse(input: &'a [u8], default: Option<Self>) -> EbmlResult<Self> {
        get_elem_data::<ID>(input)
    }
}

impl<'a, const ID: u32> EbmlParsable<'a, ID> for Uuid {
    fn parse(input: &'a [u8], default: Option<Self>) -> EbmlResult<Self> {
        map(<[u8; 16] as EbmlParsable>::parse, Uuid::from_bytes)(input)
    }
}

// FIXME: Better error handling (via append?)
pub fn get_required<T>(val: Option<T>, id: u32) -> Result<T, ErrorKind> {
    val.ok_or_else(|| {
        log::error!("Required Element {id:#0X} missing");
        ErrorKind::MissingElement
    })
}

// pub fn ebml_element<'a, O: EbmlParsable<'a>>(id: u32) -> impl Fn(&'a [u8]) -> EbmlResult<'a, O> {
//     move |i| {
//         let (i, mut size) = complete(preceded(check_id(id), elem_size))(i)?;
//         let (i, crc) = if O::has_crc() { crc(i)? } else { (i, None) };

//         if crc.is_some() {
//             // The CRC-32 Element is 6 bytes long,
//             // and we already consumed them above.
//             size -= 6;
//         }

//         let (i, data) = checksum(crc, complete(take(size)))(i)?;
//         match O::parse(data) {
//             Ok(o) => Ok((i, o)),
//             Err(kind) => ebml_err(id, kind),
//         }
//     }
// }

pub fn get_elem_data<'a, const ID: u32>(input: &'a [u8]) -> EbmlResult<&'a [u8]> {
    map(preceded(check_id(ID), elem_size), take)(input)
}

pub fn check_id<'a>(id: u32) -> impl Fn(&'a [u8]) -> EbmlResult<'a, u32> {
    move |input| {
        let (i, o) = vid(input)?;

        if id == o {
            Ok((i, o))
        } else {
            ebml_err(id, ErrorKind::MissingElement)
        }
    }
}

pub fn void(input: &[u8]) -> EbmlResult<&[u8]> {
    <&[u8] as EbmlParsable<0xEC>>::parse(input, None)
}

/// Consumes an entire EBML Element, and returns the ID if successful.
pub fn skip_element(input: &[u8]) -> EbmlResult<u32> {
    let (i, (id, size, crc)) = tuple((vid, elem_size, crc))(input)?;
    let size = if crc.is_some() { size - 6 } else { size };
    let (i, _) = checksum(crc, take(size))(i)?;
    Ok((i, id))
}

const CRC: Crc<u32> = Crc::<u32>::new(&Algorithm {
    init: 0xFFFFFFFF,
    ..crc::CRC_32_ISO_HDLC
});

pub fn crc(input: &[u8]) -> EbmlResult<Option<u32>> {
    opt(map(<[u8; 4] as EbmlParsable<0xBF>>::parse(input, None), u32::from_le_bytes))(input)
}

pub fn checksum<'a, F>(
    crc: Option<u32>,
    mut inner: F,
) -> impl FnMut(&'a [u8]) -> EbmlResult<'a, &'a [u8]>
where
    F: Parser<&'a [u8], &'a [u8], Error>,
{
    move |input| {
        let (i, o) = inner.parse(input)?;

        // FIXME: don't just return an error, the spec has well-defined CRC error handling
        match crc {
            Some(cs) if cs != CRC.checksum(o) => ebml_err(0, ErrorKind::Crc32Mismatch),
            _ => Ok((i, o)),
        }
    }
}

pub fn vint(input: &[u8]) -> EbmlResult<u64> {
    // TODO: maybe map_err to Needed::Unknown?
    let (i, first) = u8(input)?;
    let len = 1 + first.leading_zeros() as usize;

    if len > 8 {
        return ebml_err(0, ErrorKind::VintTooWide);
    }

    let (i, data) = take(len - 1)(i)?;
    let init = (first ^ (1 << (8 - len))) as u64;

    let res = data.iter().fold(init, |acc, &b| acc << 8 | b as u64);

    Ok((i, res))
}

/// Creates a [prim@u32] from the bitstream representaton of an EBML Unsigned Integer Element.
/// Note that this requires a data length of 4 bytes or less, contrary to the general EBML specification
/// of up to 8 octets per Unsigned Integer Element.
///
/// You most likely want to use [uint()] instead.
///
/// # Panics
/// - If data.len() is zero or greater than 4
///
/// # Examples
/// See [uint()].
pub fn uint_short(data: &[u8]) -> u32 {
    assert!(!data.is_empty());
    assert!(data.len() <= 4);

    data.iter().fold(0, |acc, &b| (acc << 8) | b as u32)
}

/// Creates a [prim@u64] from the bitstream representaton of an EBML Unsigned Integer Element.
///
/// # Panics
/// - If data.len() is zero or greater than 8
///
/// # Examples
/// Parsing a 5-byte unsigned integer:
/// ```
/// use matroska::ebml::uint;
///
/// let data = [10, 42, 0, 42, 42];
/// assert_eq!(43_654_326_826, uint(&data));
/// ```
///
/// Trying to parse a slice that's too long will panic:
/// ```should_panic
/// use matroska::ebml::uint;
///
/// let data = [12, 34, 56, 78, 90, 09, 87, 65, 43, 21];
/// uint(&data);
/// ```
pub fn uint(data: &[u8]) -> u64 {
    assert!(!data.is_empty());
    assert!(data.len() <= 8);

    data.iter().fold(0, |acc, &b| (acc << 8) | b as u64)
}

/// Creates an [prim@i64] from the bitstream representation of an EBML Signed Integer Element.
///
/// This function will panic for invalid data lengths, so you should ensure correct input data and handle
/// errors before calling this function.
///
/// # Panics
/// - If data.len() is zero or greater than 8
///
/// # Examples
/// Parsing a 3-byte signed integer:
/// ```
/// use matroska::ebml::int;
///
/// let data = [255, 255, 214];
/// assert_eq!(-42, int(&data));
/// ```
///
/// Trying to parse an empty slice will panic:
/// ```should_panic
/// use matroska::ebml::int;
///
/// let data = [];
/// int(&data);
/// ```
pub fn int(data: &[u8]) -> i64 {
    assert!(!data.is_empty());
    assert!(data.len() <= 8);

    let negative = (data[0] & 0b1000_0000) != 0;

    let init = if negative {
        // set all bits to one so we don't need to
        // handle "empty bytes" at the end
        -1
    } else {
        0
    };

    data.iter().fold(init, |acc, &b| acc << 8 | b as i64)
}

// The take combinator can only accept `usize`, so we need to make
// sure that the `vint` fits inside those bounds.
pub fn elem_size(input: &[u8]) -> EbmlResult<usize> {
    map_res(vint, |u| {
        usize::try_from(u).map_err(|_| {
            log::error!("Element Data Size does not fit into usize");
            Error {
                id: 0,
                kind: ErrorKind::ElementTooLarge,
            }
        })
    })(input)
}

// The ID are represented in the specification as their binary representation
// do not drop the marker bit.
pub fn vid(input: &[u8]) -> EbmlResult<u32> {
    if input.is_empty() {
        return Err(Incomplete(Needed::Unknown));
    }

    let len = 1 + input[0].leading_zeros() as usize;

    if input.len() < len {
        return Err(Incomplete(Needed::new(len - input.len())));
    }

    if len <= 4 {
        map(take(len), uint_short)(input)
    } else {
        ebml_err(0, ErrorKind::IDTooWide)
    }
}
