use std::{io::Write, num::NonZeroU32};

use cookie_factory::{
    bytes::{be_f64, be_i16, be_u8},
    combinator::slice,
    GenError, GenResult, SerializeFn, WriteContext,
};

use crate::elements::{Lacing, SimpleBlock};

/// This trait allows Rust data types to be serialized into
/// a valid representation of certain [EBML Element Types].
///
/// [EBML Element Types]: https://www.rfc-editor.org/rfc/rfc8794.html#name-ebml-element-types
pub trait EbmlSerializable {
    /// Serializes the Rust value as zero or more EBML Elements made
    /// up of an Element ID, the Element Data Size and the Element Data.
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W>;
}

// FIXME: Calc size
impl EbmlSerializable for u64 {
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        let sz = 8;
        let w = vid(id)(w)?;
        let w = vint(sz as u64)(w)?;
        slice(&self.to_be_bytes()[8 - sz..])(w)
    }
}

// FIXME: Calc size
impl EbmlSerializable for u32 {
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        let sz = 4;
        let w = vid(id)(w)?;
        let w = vint(sz as u64)(w)?;
        slice(&self.to_be_bytes()[4 - sz..])(w)
    }
}

// FIXME: Calc size
impl EbmlSerializable for i64 {
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        let sz = 8;
        let w = vid(id)(w)?;
        let w = vint(sz as u64)(w)?;
        slice(&self.to_be_bytes()[8 - sz..])(w)
    }
}

// FIXME: How do we even do this for 4-octet floats?
// I think we need a newtype...
impl EbmlSerializable for f64 {
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        let w = vid(id)(w)?;
        let w = vint(8)(w)?;
        be_f64(*self)(w)
    }
}

impl EbmlSerializable for crate::ebml::Date {
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        self.0.serialize(w, id)
    }
}

impl EbmlSerializable for String {
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        self.as_bytes().serialize(w, id)
    }
}

impl<const N: usize> EbmlSerializable for [u8; N] {
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        self.as_slice().serialize(w, id)
    }
}

impl<T: EbmlSerializable> EbmlSerializable for Vec<T> {
    fn serialize<W: Write>(&self, mut w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        for t in self {
            w = t.serialize(w, id)?;
        }

        Ok(w)
    }
}

impl<T: EbmlSerializable> EbmlSerializable for Option<T> {
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        match self {
            Some(t) => t.serialize(w, id),
            None => Ok(w),
        }
    }
}

impl EbmlSerializable for Vec<u8> {
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        self.as_slice().serialize(w, id)
    }
}

impl<'a> EbmlSerializable for &'a [u8] {
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        let w = vid(id)(w)?;
        let w = vint(self.len() as u64)(w)?;
        slice(self)(w)
    }
}

impl EbmlSerializable for uuid::Uuid {
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        self.as_bytes().serialize(w, id)
    }
}

// pub fn ebml_element<W: Write, E: EbmlSerializable>(id: NonZeroU32, elem: E) -> impl SerializeFn<W> {
//     move |output| {
//         let w = vid(id)(output)?;
//         let w = vint(elem.data_size())(w)?;
//         match elem.serialize_data(w) {
//             Ok(w) => Ok(w),
//             Err(_) => Err(GenError::CustomError(0)),
//         }
//     }
// }

pub fn vint_size(val: u64) -> Result<u8, GenError> {
    for i in 1..=8 {
        if val >> (i * 7) == 0 {
            return Ok(i);
        }
    }

    // val is too large for VINT
    Err(GenError::CustomError(0))
}

pub fn vint<W: Write>(val: u64) -> impl SerializeFn<W> {
    move |output| {
        let size = vint_size(val)?;
        let shortened = &mut val.to_be_bytes()[8 - size as usize..];

        // set VINT_MARKER
        shortened[0] |= 1 << (8 - size);

        // disable incorrect clippy warning
        #[allow(clippy::let_and_return)]
        let r = slice(shortened)(output);
        r
    }
}

pub const fn vid<W: Write>(id: NonZeroU32) -> impl SerializeFn<W> {
    move |output| {
        let skip = 4 - vid_size(id) as usize;
        slice(&id.get().to_be_bytes()[skip..])(output)
    }
}

pub const fn vid_size(i: NonZeroU32) -> u8 {
    4 - (i.leading_zeros() / 8) as u8
}

pub fn segment_element<W: Write>(w: WriteContext<W>) -> GenResult<W> {
    let w = vid(NonZeroU32::new(0x18538067).unwrap())(w)?;
    be_u8(0xFF)(w)
}

pub(crate) fn simple_block_header<W: Write>(s: &SimpleBlock) -> impl SerializeFn<W> + '_ {
    move |w| {
        let w = vint(s.track_number)(w)?;
        let w = be_i16(s.timestamp)(w)?;

        let mut flags = 0;

        if s.keyframe {
            flags |= 0b0000_0001;
        }

        if s.invisible {
            flags |= 0b0001_0000;
        }

        flags |= match s.lacing {
            Lacing::None => 0,
            Lacing::Xiph => 0b0010_0000,
            Lacing::FixedSize => 0b0100_0000,
            Lacing::EBML => 0b0110_0000,
        };

        if s.discardable {
            flags |= 0b1000_0000;
        }

        be_u8(flags)(w)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vint_size_correct() {
        let tests: [(u64, Option<u8>); 7] = [
            (0, Some(1)),
            (100, Some(1)),
            ((1 << 7) - 1, Some(1)),
            (1 << 7, Some(2)),
            ((1 << 56) - 1, Some(8)),
            (1 << 56, None),
            (u64::MAX, None),
        ];

        for (val, expected_size) in tests {
            let actual_size = vint_size(val).ok();
            assert_eq!(
                actual_size, expected_size,
                "VINT size of value {val} is {actual_size:?}, but should be {expected_size:?}."
            );
        }
    }

    #[test]
    fn vint_correct() {
        let tests: [(u64, Option<[u8; 8]>); 8] = [
            (0, Some([0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
            (1, Some([0x81, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
            (127, Some([0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
            (128, Some([0x40, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
            (
                1 << 35,
                Some([0x04, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ),
            (
                (1 << 56) - 1,
                Some([0x01, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
            ),
            (1 << 56, None),
            (u64::MAX, None),
        ];

        for (val, expected_output) in tests {
            let mut buf = [0u8; 8];
            let w = buf.as_mut_slice().into();

            // If vint() fails, expected_output == None
            assert_eq!(vint(val)(w).is_ok(), expected_output.is_some());

            if let Some(expected_buf) = expected_output {
                assert_eq!(buf, expected_buf);
            }
        }
    }
}

