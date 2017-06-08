open BooleanAlgebraSig

(**TensorProductOfBooleanAlgebra*)
module TensorProduct(BA:BooleanAlgebra):Graded with type generator = BA.t

(**TensorProductOfTopologicalBooleanAlgebra*)
module TopologicalTensorProduct(BA:Topological):GradedTopological with type generator = BA.t

(**TensorProductOfDirectedBooleanAlgebra*)
module DirectedTensorProduct(BA:Directed):GradedDirected with type generator = BA.t
