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

// tests
#[cfg(test)]
mod tests {

    use super::*;
    #[test]
    fn parse_test_from_txt() {
        let txt = include_str!("../tests1.txt");
        let parsed: Vec<_> = parser::parse(txt).unwrap();
        for module in parsed {
            println!("{}", module);
        }
    }
    #[test]
    fn elaborate_test() {
        let txt = include_str!("../tests1.txt");
        let parsed: Vec<_> = parser::parse(txt).unwrap();
        let mut elaborator = elaborator::Elaborator::new();

        for module in parsed {
            elaborator.add_to_root_module(module).unwrap();
        }

        // tree of modules
        let root = elaborator.root();
        println!("------");
        printing::print_traverse_module_tree(root);
    }
}
