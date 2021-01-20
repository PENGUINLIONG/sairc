use std::rc::Rc;



struct AstInner {
    ast: Option<Rc<Ast>>,
}
struct Ast(Rc<AstInner>);



struct LoadCommand {
    id: InstrId,
    data_name: String,
    rngs: Vec<Range>,
}
struct StoreCommand {
    id: InstrId,
    data_name: String,
    rngs: Vec<Range>,
    x_id: InstrId,
}
struct IndexCommand {
    id: InstrId,
    rngs: Vec<Range>,
    x_id: InstrId,
}
struct MapCommand {
    id: InstrId,
    op: String,
    x_ids: Vec<InstrId>,
}
struct ReduceCommand {
    id: InstrId,
    op: String,
    x_id: InstrId,
    dim_idxs: Vec<usize>,
}

enum Command {
    Load(LoadCommand),
    Store(StoreCommand),
    Index(IndexCommand),
    Map(MapCommand),
    Reduce(ReduceCommand),
    Fence,
}

pub struct Data {
    shape: Vec<usize>,
    pathway: String,
}
pub struct DataDescriptor {

}

#[derive(Clone, Debug, Default)]
struct Program {
    // Commands executed in the program in sequential order.
    cmds: Vec<Command>,
    // Data blocks declares in the program.
    datas: Vec<Data>,
    // Root ranges declared in the program.
    rngs: Vec<Range>,
}
impl Program {
    pub fn new() -> Self {

    }
}
