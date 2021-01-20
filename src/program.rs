use std::rc::Rc;
use std::collections::HashMap;

type InstrId = usize;
type DataId = InstrId;
type RangeId = InstrId;

type DimIdx = usize;

#[derive(Clone, Debug)]
struct RangeCommand {
    dst_id: RangeId,
    name: String,
    offset: usize,
    len: usize,
}
#[derive(Clone, Debug)]
struct SplitCommand {
    dst_far_id: RangeId,
    dst_near_id: RangeId,
    src_id: RangeId,
}
#[derive(Clone, Debug)]
struct MergeCommand {
    dst_id: RangeId,
    src_far_id: RangeId,
    src_near_id: RangeId,
}

#[derive(Clone, Debug)]
struct LoadCommand {
    dst_id: DataId,
    shape: Vec<usize>,
    name: String,
}
#[derive(Clone, Debug)]
struct StoreCommand {
    dst_id: DataId,
    src_id: DataId,
    name: String,
}
#[derive(Clone, Debug)]
struct InpdexCommand {
    dst_id: DataId,
    src_id: DataId,
    src_rngs: Vec<RangeId>,
}
#[derive(Clone, Debug)]
struct BroadcastCommand {
    dst_id: DataId,
    src_id: DataId,
    dim_idx: DimIdx,
    len: usize,
}
#[derive(Clone, Debug)]
struct MapCommand {
    op: String,
    dst_id: DataId,
    src_ids: Vec<DataId>,
}
#[derive(Clone, Debug)]
struct ReduceCommand {
    op: String,
    dst_id: DataId,
    src_id: DataId,
    dim_idx: DimIdx,
}

#[derive(Clone, Debug)]
enum Command {
    Range(RangeCommand),
    Split(SplitCommand),
    Merge(MergeCommand),

    Load(LoadCommand),
    Store(StoreCommand),
    Broadcast(BroadcastCommand),
    Map(MapCommand),
    Reduce(ReduceCommand),

    Fence,
}

#[derive(Clone, Debug)]
enum DataAccessType {
    Intermediate,
    Input,
    Output,
}
impl Default for DataAccessType {
    fn default() -> Self {
        DataAccessType::Intermediate
    }
}

#[derive(Clone, Debug, Default)]
pub struct Data {
    name: String,
    shape: Vec<usize>,

    is_root: bool,
    access_ty: DataAccessType,
}

#[derive(Clone, Debug, Default)]
pub struct Range {
    name: String,
    offset: usize,
    len: usize,

    is_root: bool,
    is_reduced: bool,
}

#[derive(Clone, Debug, Default)]
struct Program {
    // Commands executed in the program in sequential order.
    cmds: Vec<Command>,
    // Data blocks declares in the program.
    data_map: HashMap<DataId, Data>,
    // Root ranges declared in the program.
    rng_map: HashMap<RangeId, Range>,
}
impl Program {
    pub fn new() -> Program {
        let cmds = vec![
            Command::Load(LoadCommand {
                dst_id: 0,
                shape: vec![3, 5],
                name: "A".to_owned(),
            }),
            Command::Load(LoadCommand {
                dst_id: 1,
                shape: vec![5, 7],
                name: "B".to_owned(),
            }),

            Command::Range(RangeCommand {
                dst_id: 3,
                name: "m".to_owned(),
                offset: 0,
                len: 3,
            }),
            Command::Range(RangeCommand {
                dst_id: 4,
                name: "n".to_owned(),
                offset: 0,
                len: 7,
            }),
            Command::Range(RangeCommand {
                dst_id: 5,
                name: "k".to_owned(),
                offset: 0,
                len: 5,
            }),

            Command::Broadcast(BroadcastCommand {
                dst_id: 6,
                src_id: 0,
                dim_idx: 2,
                len: 7,
            }),
            Command::Broadcast(BroadcastCommand {
                dst_id: 7,
                src_id: 1,
                dim_idx: 0,
                len: 3,
            }),

            Command::Map(MapCommand {
                dst_id: 8,
                op: "mul".to_owned(),
                src_ids: vec![6, 7],
            }),
            Command::Reduce(ReduceCommand {
                dst_id: 9,
                op: "add".to_owned(),
                src_id: 6,
                dim_idx: 1,
            }),

            Command::Store(StoreCommand {
                dst_id: 2,
                src_id: 9,
                name: "C".to_owned(),
            }),
        ];

        let mut rv = Program::default();

        cmds.iter()
            .for_each(|cmd| rv.push_cmd(cmd));

        Program::default()
    }

    fn get_rng(&mut self, id: RangeId) -> &Range {
        self.rng_map.get(&id)
            .expect("source range is unassigned")
    }
    fn get_data(&mut self, id: DataId) -> &Data {
        self.data_map.get(&id)
            .expect("source data is unassigned")
    }

    fn insert_rng(&mut self, id: RangeId, rng: Range) {
        if self.rng_map.insert(id, rng).is_some() {
            panic!("range id conflict");
        }
    }
    fn insert_data(&mut self, id: DataId, data: Data) {
        if self.data_map.insert(id, data).is_some() {
            panic!("data id conflict");
        }
    }

    fn push_cmd(&mut self, cmd: &Command) {
        self.cmds.push(cmd.clone());

        match cmd {
            Command::Range(cmd) => {
                let rng = Range {
                    name: cmd.name.clone(),
                    offset: cmd.offset,
                    len: cmd.len,
                    is_root: true,
                    ..Default::default()
                };
                self.insert_rng(cmd.dst_id, rng);
            },
            Command::Load(cmd) => {
                let data = Data {
                    name: cmd.name.clone(),
                    shape: cmd.shape.clone(),
                    is_root: true,
                    access_ty: DataAccessType::Input,
                    ..Default::default()
                };
                self.insert_data(cmd.dst_id, data);
            },
            Command::Store(cmd) => {
                if let Some(src_data) = self.data_map.get_mut(&cmd.src_id) {
                    let data = Data {
                        name: cmd.name.clone(),
                        shape: src_data.shape.clone(),
                        access_ty: DataAccessType::Output,
                        ..Default::default()
                    };
                    self.insert_data(cmd.dst_id, data);
                } else {
                    panic!("source data is unassigned");
                }
            },
            Command::Broadcast(cmd) => {
                if let Some(src_data) = self.data_map.get_mut(&cmd.src_id) {
                    let shape = src_data.clone();
                    let data = Data {
                        shape,
                    }
                }
            }
            _ => unimplemented!(),
        }

    }
}
