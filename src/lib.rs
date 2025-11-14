// simply typed lambda calculus kernel
mod kernel;
// simply typed lambda calculus surface
//   - before name resolution and desugaring
//   - includes nested modules and module parameters
mod surface;
// string -> surface
mod parser;
// 3 kinds of elaborators
// module system
mod module_system;
// term elaboration
mod term_elaborator;
// resolver
mod resolver;
