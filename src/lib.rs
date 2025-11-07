// simply typed lambda calculus kernel
mod kernel;
// simply typed lambda calculus surface
//   - before name resolution and desugaring
//   - includes nested modules and module parameters
mod surface;
// string -> surface
mod parser;
// surface -> kernel elaborator
mod elaborator;
