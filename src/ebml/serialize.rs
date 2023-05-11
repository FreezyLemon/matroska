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

impl EbmlSerializable for u64 {
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        let mut sz = 8 - (self.leading_zeros() / 8) as usize;

        // FIXME: Allow zero-sized uints
        if sz == 0 {
            sz = 1;
        }
        let w = vid(id)(w)?;
        let w = vint(sz as u64)(w)?;
        slice(&self.to_be_bytes()[8 - sz..])(w)
    }
}

impl EbmlSerializable for u32 {
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        (*self as u64).serialize(w, id)
    }
}

impl EbmlSerializable for i64 {
    fn serialize<W: Write>(&self, w: WriteContext<W>, id: NonZeroU32) -> GenResult<W> {
        if *self >= 0 {
            return (*self as u64).serialize(w, id);
        }

        // leading_ones() - 1 because of the sign bit
        let sz = 8 - ((self.leading_ones() - 1) / 8) as usize;
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

    #[test]
    fn uint_correct() {
        // To explain the expected outputs, let's use the third test as an example:
        // (val: 0xFF, id: 0x84) should result in: [0x84, 0x81, 0xFF, 0x00, /* more zeros */],
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
        let tests: [((u64, u32), [u8; 13]); 5] = [
            ((0, 0x81), [0x81, 0x81, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ((1, 0x90), [0x90, 0x81, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ((0xFF, 0x84), [0x84, 0x81, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (((4321 << 42) + 8765, 0x12AB_34CD), [0x12, 0xAB, 0x34, 0xCD, 0x87, 0x43, 0x84, 0x00, 0x00, 0x00, 0x22, 0x3D, 0x00]),
            ((u64::MAX, 0xABCD_EFFE), [0xAB, 0xCD, 0xEF, 0xFE, 0x88, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
        ];

        for ((val, id), expected_output) in tests {
            let mut buf = [0u8; 13];
            let w = buf.as_mut_slice().into();

            // If try_serialize fails, expected_output == None
            assert!(
                val.serialize(w, NonZeroU32::new(id).unwrap()).is_ok(),
                "serialization failed for id: {id:#0X}"
            );

            assert_eq!(buf, expected_output, "id: {id:#0X}");
        }
    }

    #[test]
    fn int_correct() {
        // see uint_correct for an explanation of what's happening here
        #[rustfmt::skip]
        let tests: [((i64, u32), [u8; 13]); 11] = [
            // x >= 0 -> equal to uint:
            ((0, 0x81), [0x81, 0x81, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ((1, 0x90), [0x90, 0x81, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ((0xFF, 0x84), [0x84, 0x81, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (((4321 << 42) + 8765, 0x12AB_34CD), [0x12, 0xAB, 0x34, 0xCD, 0x87, 0x43, 0x84, 0x00, 0x00, 0x00, 0x22, 0x3D, 0x00]),
            ((i64::MAX, 0xABCD_EFFE), [0xAB, 0xCD, 0xEF, 0xFE, 0x88, 0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),

            // x < 0 -> more tests
            ((i64::MIN, 0x82), [0x82, 0x88, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ((-1, 0x83), [0x83, 0x81, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ((-128, 0x84), [0x84, 0x81, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ((-129, 0x85), [0x85, 0x82, 0xFF, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ((-87654321, 0x86), [0x86, 0x84, 0xFA, 0xC6, 0x80, 0x4F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            (((-4321 << 42) - 8765, 0x87), [0x87, 0x87, 0xBC, 0x7B, 0xFF, 0xFF, 0xFF, 0xDD, 0xC3, 0x00, 0x00, 0x00, 0x00]),
        ];

        for ((val, id), expected_output) in tests {
            let mut buf = [0u8; 13];
            let w = buf.as_mut_slice().into();

            // If try_serialize fails, expected_output == None
            assert!(
                val.serialize(w, NonZeroU32::new(id).unwrap()).is_ok(),
                "serialization failed for id: {id:#0X}"
            );

            assert_eq!(buf, expected_output, "id: {id:#0X}");
        }
    }
}
