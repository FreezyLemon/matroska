use std::io::Write;
use std::sync::Arc;

use cookie_factory::SerializeFn;

use av_data::{packet::Packet, params::MediaKind, value::Value};
use av_format::{common::GlobalInfo, error::*, muxer::*, stream::Stream};

use crate::ebml::serialize;
use crate::{
    ebml::{EbmlHeader, EbmlSerializable},
    elements::{
        Audio, Cluster, Colour, Info, Lacing, SeekHead, SimpleBlock, TrackEntry, TrackType, Tracks,
        Video,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub struct MkvMuxer {
    header: EbmlHeader,
    seek_head: SeekHead,
    info: Option<Info>,
    tracks: Option<Tracks>,
    blocks: Vec<Vec<u8>>,
    blocks_len: usize,
    timestamp: Option<u64>,
}

impl MkvMuxer {
    pub fn matroska() -> MkvMuxer {
        MkvMuxer {
            header: EbmlHeader {
                version: 1,
                read_version: 1,
                max_id_length: 4,
                max_size_length: 8,
                doc_type: String::from("matroska"),
                doc_type_version: 4,
                doc_type_read_version: 2,
            },
            seek_head: SeekHead {
                positions: Vec::new(),
            },
            info: None,
            tracks: None,
            blocks: Vec::new(),
            blocks_len: 0,
            timestamp: None,
        }
    }

    pub fn webm() -> MkvMuxer {
        MkvMuxer {
            header: EbmlHeader {
                version: 1,
                read_version: 1,
                max_id_length: 4,
                max_size_length: 8,
                doc_type: String::from("webm"),
                doc_type_version: 1,
                doc_type_read_version: 1,
            },
            seek_head: SeekHead {
                positions: Vec::new(),
            },
            info: None,
            tracks: None,
            blocks: Vec::new(),
            blocks_len: 0,
            timestamp: None,
        }
    }

    pub fn write_ebml_header(&mut self, buf: &mut Vec<u8>) -> Result<()> {
        let written = try_write(buf, |w| self.header.serialize::<0x1A45DFA3, _>(w, None))?;
        buf.truncate(written as usize);

        Ok(())
    }

    pub fn write_segment_header(&mut self, buf: &mut Vec<u8>, _size: usize) -> Result<()> {
        let written = try_write(buf, serialize::segment_element)?;
        buf.truncate(written as usize);

        Ok(())
    }

    pub fn write_seek_head(&mut self, buf: &mut Vec<u8>) -> Result<()> {
        let written = try_write(buf, |w| self.seek_head.serialize::<0x114D9B74, _>(w, None))?;
        buf.truncate(written as usize);

        Ok(())
    }

    pub fn write_info(&mut self, buf: &mut Vec<u8>) -> Result<()> {
        if let Some(ref info) = self.info {
            let written = try_write(buf, |w| info.serialize::<0x1549A966, _>(w, None))?;
            buf.truncate(written as usize);
        }
        Ok(())
    }

    pub fn write_tracks(&mut self, buf: &mut Vec<u8>) -> Result<()> {
        if let Some(ref tracks) = self.tracks {
            let written = try_write(buf, |w| tracks.serialize::<0x1654AE6B, _>(w, None))?;
            buf.truncate(written as usize);
        }
        Ok(())
    }
}

impl Muxer for MkvMuxer {
    fn configure(&mut self) -> Result<()> {
        Ok(())
    }

    fn write_header<W: Write>(&mut self, out: &mut Writer<W>) -> Result<()> {
        let mut buf = Vec::new();
        let mut ebml_header = Vec::new();
        self.write_ebml_header(&mut ebml_header)?;
        let mut segment_header = Vec::new();
        //127 corresponds to unknown size
        self.write_segment_header(&mut segment_header, 127)?;

        buf.extend_from_slice(&ebml_header);
        buf.extend_from_slice(&segment_header);

        let mut info = Vec::new();
        self.write_info(&mut info)?;
        let mut tracks = Vec::new();
        self.write_tracks(&mut tracks)?;

        // FIXME: Reintroduce this
        // let info_seek = Seek {
        //     id: [0x15, 0x49, 0xA9, 0x66],
        //     position: 0,
        // };
        // let tracks_seek = Seek {
        //     id: [0x16, 0x54, 0xAE, 0x6B],
        //     position: 0,
        // };
        // let cluster_seek = Seek {
        //     id: [0x1F, 0x43, 0xB6, 0x75],
        //     position: 0,
        // };

        // self.seek_head.positions.push(info_seek);
        // self.seek_head.positions.push(tracks_seek);
        // self.seek_head.positions.push(cluster_seek);

        // self.seek_head.positions[0].position = self.seek_head.size(0x114D9B74) as u64;
        // self.seek_head.positions[1].position =
        //     (self.seek_head.size(0x114D9B74) + info.size(0x1549A966)) as u64;
        // self.seek_head.positions[2].position = (self.seek_head.size(0x114D9B74)
        //     + info.size(0x1549A966)
        //     + tracks.size(0x1654AE6B)) as u64;

        let mut seek_head = Vec::new();
        self.write_seek_head(&mut seek_head)?;

        buf.extend_from_slice(&seek_head);
        buf.extend_from_slice(&info);
        buf.extend_from_slice(&tracks);

        out.write_all(&buf).unwrap();

        Ok(())
    }

    fn write_packet<W: Write>(&mut self, out: &mut Writer<W>, pkt: Arc<Packet>) -> Result<()> {
        let mut v = Vec::with_capacity(16);

        let s = SimpleBlock {
            track_number: pkt.stream_index as u64 + 1,
            timestamp: pkt.t.pts.or(pkt.t.dts).unwrap_or(0) as i16,
            keyframe: pkt.is_key,
            invisible: false,
            lacing: Lacing::None,
            discardable: false,
        };

        let written = try_write(&mut v, serialize::simple_block_header(&s))?;
        v.truncate(written as usize);

        v.extend(pkt.data.iter());
        let len = v.len();
        self.blocks.push(v);
        self.blocks_len += len;

        self.timestamp = if self.timestamp.is_none() {
            Some(pkt.t.pts.or(pkt.t.dts).unwrap_or(0) as u64)
        } else {
            return Err(Error::InvalidData);
        };

        if pkt.is_key || self.blocks_len >= 5242880 {
            {
                let simple_blocks: Vec<&[u8]> = self.blocks.iter().map(|v| &v[..]).collect();

                let cluster = Cluster {
                    timestamp: self.timestamp.take().unwrap(),
                    position: None,
                    prev_size: None,
                    simple_block: simple_blocks,
                    block_group: Vec::new(),
                };

                let mut buf = vec![0u8; 16];
                let written = try_write(&mut buf, |w| cluster.serialize::<0x1F43B675, _>(w, None))?;
                buf.truncate(written as usize);
                out.write_all(&buf).unwrap();
            }

            self.blocks.truncate(0);
            self.blocks_len = 0;
        }

        Ok(())
    }

    fn write_trailer<W: Write>(&mut self, out: &mut Writer<W>) -> Result<()> {
        let nb = self.blocks.len();

        if nb > 0 {
            let simple_blocks: Vec<&[u8]> = self.blocks.iter().map(|v| &v[..]).collect();

            let cluster = Cluster {
                timestamp: self.timestamp.take().unwrap(),
                position: None,
                prev_size: None,
                simple_block: simple_blocks,
                block_group: Vec::new(),
            };

            let mut buf = vec![0u8; 16];
            let written = try_write(&mut buf, |w| cluster.serialize::<0x1F43B675, _>(w, None))?;
            buf.truncate(written as usize);

            out.write_all(&buf).unwrap();
        }

        Ok(())
    }

    fn set_global_info(&mut self, info: GlobalInfo) -> Result<()> {
        self.tracks = Some(Tracks {
            tracks: info.streams.iter().map(stream_to_track).collect(),
        });

        self.info = Some(Info {
            muxing_app: String::from("rust-av"),
            writing_app: String::from("rust-av"),
            duration: info.duration.map(|d| d as f64),
            timestamp_scale: 1000000,
            ..Default::default()
        });

        Ok(())
    }

    fn set_option(&mut self, _key: &str, _val: Value<'_>) -> Result<()> {
        Ok(())
    }
}

pub fn stream_to_track(s: &Stream) -> TrackEntry {
    let codec_id = match s.params.codec_id.as_deref() {
        Some("opus") => String::from("A_OPUS"),
        Some("vorbis") => String::from("A_VORBIS"),
        Some("av1") => String::from("V_AV1"),
        Some("vp8") => String::from("V_VP8"),
        Some("vp9") => String::from("V_VP9"),
        _ => String::from("INVALID_CODEC"),
    };

    let mut t = TrackEntry {
        track_uid: s.id as u64,
        track_number: s.index as u64 + 1,
        track_type: 0,
        codec_id,
        default_duration: s.duration,
        codec_delay: s.params.delay as u64,
        codec_private: s.params.extradata.clone(),
        seek_pre_roll: s.params.convergence_window as u64,
        ..Default::default()
    };

    match s.params.kind {
        Some(MediaKind::Video(ref v)) => {
            t.track_type = TrackType::Video.into();
            t.video = Some(Video {
                pixel_width: v.width as u64,
                pixel_height: v.height as u64,
                colour: Some(Colour {
                    matrix_coefficients: match v.format.as_ref() {
                        Some(fmt) => fmt.get_matrix() as u64,
                        None => 0u64,
                    },
                    transfer_characteristics: match v.format.as_ref() {
                        Some(fmt) => fmt.get_xfer() as u64,
                        None => 0u64,
                    },
                    primaries: match v.format.as_ref() {
                        Some(fmt) => fmt.get_primaries() as u64,
                        None => 0u64,
                    },
                    ..Default::default()
                }),
                ..Default::default()
            });
        }
        Some(MediaKind::Audio(ref a)) => {
            t.track_type = TrackType::Audio.into();
            t.audio = Some(Audio {
                sampling_frequency: a.rate as f64,
                channels: 1,
                ..Default::default()
            });
        }
        _ => {}
    }

    t
}

fn try_write<'a, F>(buf: &'a mut Vec<u8>, f: F) -> Result<u64>
where
    F: SerializeFn<&'a mut Vec<u8>>,
{
    match f(buf.into()) {
        Ok(ctx) => Ok(ctx.position),
        Err(_) => Err(Error::InvalidData),
    }
}
