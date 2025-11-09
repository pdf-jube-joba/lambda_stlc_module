// simply typed lambda calculus kernel
pub mod kernel;
// simply typed lambda calculus surface
//   - before name resolution and desugaring
//   - includes nested modules and module parameters
pub mod surface;
// surface/kernel pretty printing
pub mod printing;
// string -> surface
pub mod parser;
// surface -> kernel elaborator
pub mod elaborator;
