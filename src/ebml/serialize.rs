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
pub trait EbmlSerializable: Sized {
    /// Serializes the Rust value as zero or more EBML Elements made
    /// up of an Element ID, the Element Data Size and the Element Data.
    fn serialize<const ID: u32, W: Write>(
        &self,
        w: WriteContext<W>,
        default: Option<Self>,
    ) -> GenResult<W>;
}

impl EbmlSerializable for u64 {
    fn serialize<const ID: u32, W: Write>(
        &self,
        w: WriteContext<W>,
        default: Option<Self>,
    ) -> GenResult<W> {
        let default = default.unwrap_or(0);

        let sz = match *self {
            // don't serialize if val == default
            s if s == default => 0,

            // we need to serialize zeros if the default is != 0
            0 => 1,

            // calculate minimum required size
            s => 8 - (s.leading_zeros() / 8) as usize,
        };

        let w = vid::<ID, W>(w)?;
        let w = vint(sz as u64)(w)?;
        slice(&self.to_be_bytes()[8 - sz..])(w)
    }
}

impl EbmlSerializable for u32 {
    fn serialize<const ID: u32, W: Write>(
        &self,
        w: WriteContext<W>,
        default: Option<Self>,
    ) -> GenResult<W> {
        // TODO: Investigate whether this could benefit from a manual implementation
        // (copy-paste code for u64 and change 8 -> 4)
        u64::from(*self).serialize::<ID, W>(w, default.map(u64::from))
    }
}

impl EbmlSerializable for i64 {
    fn serialize<const ID: u32, W: Write>(
        &self,
        w: WriteContext<W>,
        default: Option<Self>,
    ) -> GenResult<W> {
        let default = default.unwrap_or(0);

        let sz = match *self {
            // don't serialize if val == default
            s if s == default => 0,

            // we need to serialize zeros if the default is != 0
            0 => 1,

            // handle positive numbers like u64s
            s @ 1.. => return (s as u64).serialize::<ID, W>(w, Some(*self as u64 - 1)),

            // two's complement -> leading_ones instead of leading_zeros
            s => 8 - ((s.leading_ones() - 1) / 8) as usize,
        };

        let w = vid::<ID, W>(w)?;
        let w = vint(sz as u64)(w)?;
        slice(&self.to_be_bytes()[8 - sz..])(w)
    }
}

// FIXME: How do we even do this for 4-octet floats?
// I think we need a newtype...
impl EbmlSerializable for f64 {
    fn serialize<const ID: u32, W: Write>(
        &self,
        w: WriteContext<W>,
        default: Option<Self>,
    ) -> GenResult<W> {
        let default = default.unwrap_or(0.0);

        let w = vid::<ID, W>(w)?;
        if *self == default {
            vint(0)(w)
        } else {
            let w = vint(8)(w)?;
            be_f64(*self)(w)
        }
    }
}

impl EbmlSerializable for crate::ebml::Date {
    fn serialize<const ID: u32, W: Write>(
        &self,
        w: WriteContext<W>,
        default: Option<Self>,
    ) -> GenResult<W> {
        // The default for Date Elements defaults to Date(0_i64).
        // The default for Integer Elements is also 0, so this is fine.
        self.0.serialize::<ID, W>(w, default.map(|d| d.0))
    }
}

impl EbmlSerializable for String {
    fn serialize<const ID: u32, W: Write>(
        &self,
        w: WriteContext<W>,
        default: Option<Self>,
    ) -> GenResult<W> {
        let default = &default.unwrap_or_default();
        if self == default {
            [].serialize::<ID, W>(w, None)
        } else {
            self.as_bytes().serialize::<ID, W>(w, None)
        }
    }
}

impl<const N: usize> EbmlSerializable for [u8; N] {
    fn serialize<const ID: u32, W: Write>(
        &self,
        w: WriteContext<W>,
        default: Option<Self>,
    ) -> GenResult<W> {
        assert!(default.is_none(), "Default values are not supported for generic Binary Elements. If you are parsing the binary and want to supply a default value for the parsed type, consider implementing a newtype.");

        self.as_slice().serialize::<ID, W>(w, None)
    }
}

impl<T: EbmlSerializable> EbmlSerializable for Vec<T> {
    fn serialize<const ID: u32, W: Write>(
        &self,
        mut w: WriteContext<W>,
        default: Option<Self>,
    ) -> GenResult<W> {
        assert!(
            default.is_none(),
            "Default values are not supported for Vec<T>"
        );

        for t in self {
            w = t.serialize::<ID, W>(w, None)?;
        }

        Ok(w)
    }
}

impl<T: EbmlSerializable> EbmlSerializable for Option<T> {
    fn serialize<const ID: u32, W: Write>(
        &self,
        w: WriteContext<W>,
        default: Option<Self>,
    ) -> GenResult<W> {
        assert!(default.is_none(), "Default values are not supported for Option<T>. Optional Elements with a default value should be represented by T, not Option<T>");

        match self {
            Some(t) => t.serialize::<ID, W>(w, None),
            None => Ok(w),
        }
    }
}

impl EbmlSerializable for Vec<u8> {
    fn serialize<const ID: u32, W: Write>(
        &self,
        w: WriteContext<W>,
        default: Option<Self>,
    ) -> GenResult<W> {
        assert!(default.is_none(), "Default values are not supported for generic Binary Elements. If you are parsing the binary and want to supply a default value for the parsed type, consider implementing a newtype.");

        self.as_slice().serialize::<ID, W>(w, None)
    }
}

impl<'a> EbmlSerializable for &'a [u8] {
    fn serialize<const ID: u32, W: Write>(
        &self,
        w: WriteContext<W>,
        default: Option<Self>,
    ) -> GenResult<W> {
        assert!(default.is_none(), "Default values are not supported for generic Binary Elements. If you are parsing the binary and want to supply a default value for the parsed type, consider implementing a newtype.");

        let w = vid::<ID, W>(w)?;
        let w = vint(self.len() as u64)(w)?;
        slice(self)(w)
    }
}

impl EbmlSerializable for uuid::Uuid {
    fn serialize<const ID: u32, W: Write>(
        &self,
        w: WriteContext<W>,
        default: Option<Self>,
    ) -> GenResult<W> {
        assert!(
            default.is_none(),
            "Default values are not supported for UIDs and UUIDs, as that wouldn't make sense."
        );

        self.as_bytes().serialize::<ID, W>(w, None)
    }
}

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

pub fn vid<const ID: u32, W: Write>(w: WriteContext<W>) -> GenResult<W> {
    assert_ne!(ID, 0);
    let id = NonZeroU32::new(ID).unwrap();
    let skip = 4 - vid_size(id) as usize;

    slice(&ID.to_be_bytes()[skip..])(w)
}

pub const fn vid_size(i: NonZeroU32) -> u8 {
    4 - (i.leading_zeros() / 8) as u8
}

pub fn segment_element<W: Write>(w: WriteContext<W>) -> GenResult<W> {
    let w = vid::<0x18538067, W>(w)?;
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
        #[rustfmt::skip]
        let tests: [(u64, Option<[u8; 8]>); 8] = [
            (0, Some([0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
            (1, Some([0x81, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
            (127, Some([0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
            (128, Some([0x40, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
            (1 << 35, Some([0x04, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
            ((1 << 56) - 1, Some([0x01, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF])),
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

    // TODO: Expand int/uint tests to include correct handling of default values

    #[test]
    fn uint_correct() {
        // To explain the expected outputs, let's use the third test as an example:
        // (val: 0xFF, expected_len: 3, id: 0x84) should result in: [0x84, 0x81, 0xFF, 0x00, /* more zeros */],
        //
        // The first byte 0x84 is the Element ID literally.
        // The second byte 0x81 is a 1-byte long VINT representing the Element Size:
        // 0b1000_0001 => The most significant ("first") bit is the VINT_MARKER, which
        // shows us that the VINT is one octet/byte long. All bits after that are inter-
        // preted as the uint value. So in this case that would be 0b000_0001, so just 1.
        //
        // This shows us that we need to "take" one more octet/byte to get the value of this
        // uint Element. So we just take the next byte (0xFF) and interpret that literally.
        //
        // If we need to "take" more than one octet/byte, we do it in Big-Endian order.
        // For us, this just means "read/write the arrays/slices in order", so index ascending.

        // max length = 4 (ID) + 1 (Size) + 8 (Data) = 13
        #[rustfmt::skip]
        let tests: [(u64, u64, [u8; 13]); 5] = [
            (0, 2, [0x81, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (1, 3, [0x90, 0x81, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (0xFF, 3, [0x84, 0x81, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ((4321 << 42) + 8765, 12, [0x12, 0xAB, 0x34, 0xCD, 0x87, 0x43, 0x84, 0x00, 0x00, 0x00, 0x22, 0x3D, 0x00]),
            (u64::MAX, 13, [0xAB, 0xCD, 0xEF, 0xFE, 0x88, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
        ];

        check_uint::<0x81>(tests[0].0, tests[0].1, tests[0].2);
        check_uint::<0x90>(tests[1].0, tests[1].1, tests[1].2);
        check_uint::<0x84>(tests[2].0, tests[2].1, tests[2].2);
        check_uint::<0x12AB_34CD>(tests[3].0, tests[3].1, tests[3].2);
        check_uint::<0xABCD_EFFE>(tests[4].0, tests[4].1, tests[4].2);
    }

    fn check_uint<const ID: u32>(val: u64, expected_len: u64, expected: [u8; 13]) {
        let mut buf = [0u8; 13];
        let w = buf.as_mut_slice().into();
        let w = val
            .serialize::<ID, _>(w, None)
            .unwrap_or_else(|_| panic!("serialization failed for id: {ID:#0X}"));

        assert_eq!(w.position, expected_len, "id: {ID:#0X}");
        assert_eq!(buf, expected, "id: {ID:#0X}");
    }

    #[test]
    fn int_correct() {
        // see uint_correct for an explanation of what's happening here
        #[rustfmt::skip]
        let tests: [(i64, u64, [u8; 13]); 11] = [
            // x >= 0 -> equal to uint:
            (0, 2, [0x81, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (1, 3, [0x90, 0x81, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (0xFF, 3, [0x84, 0x81, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ((4321 << 42) + 8765, 12, [0x12, 0xAB, 0x34, 0xCD, 0x87, 0x43, 0x84, 0x00, 0x00, 0x00, 0x22, 0x3D, 0x00]),
            (i64::MAX, 13, [0xAB, 0xCD, 0xEF, 0xFE, 0x88, 0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),

            // x < 0 -> more tests
            (i64::MIN, 10, [0x82, 0x88, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (-1, 3, [0x83, 0x81, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (-128, 3, [0x84, 0x81, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (-129, 4, [0x85, 0x82, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (-87654321, 6, [0x86, 0x84, 0xFA, 0xC6, 0x80, 0x4F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ((-4321 << 42) - 8765, 9, [0x87, 0x87, 0xBC, 0x7B, 0xFF, 0xFF, 0xFF, 0xDD, 0xC3, 0x00, 0x00, 0x00, 0x00]),
        ];

        check_int::<0x81>(tests[0].0, tests[0].1, tests[0].2);
        check_int::<0x90>(tests[1].0, tests[1].1, tests[1].2);
        check_int::<0x84>(tests[2].0, tests[2].1, tests[2].2);
        check_int::<0x12AB_34CD>(tests[3].0, tests[3].1, tests[3].2);
        check_int::<0xABCD_EFFE>(tests[4].0, tests[4].1, tests[4].2);

        check_int::<0x82>(tests[5].0, tests[5].1, tests[5].2);
        check_int::<0x83>(tests[6].0, tests[6].1, tests[6].2);
        check_int::<0x84>(tests[7].0, tests[7].1, tests[7].2);
        check_int::<0x85>(tests[8].0, tests[8].1, tests[8].2);
        check_int::<0x86>(tests[9].0, tests[9].1, tests[9].2);
        check_int::<0x87>(tests[10].0, tests[10].1, tests[10].2);
    }

    fn check_int<const ID: u32>(val: i64, expected_len: u64, expected: [u8; 13]) {
        let mut buf = [0u8; 13];
        let w = buf.as_mut_slice().into();
        let w = val
            .serialize::<ID, _>(w, None)
            .unwrap_or_else(|_| panic!("serialization failed for id: {ID:#0X}"));

        assert_eq!(w.position, expected_len, "id: {ID:#0X}");
        assert_eq!(buf, expected, "id: {ID:#0X}");
    }
}
