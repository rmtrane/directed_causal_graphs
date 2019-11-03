# Create and Simulate From Directed Graph

1. Create subdirectory under `data/` where you want to put the data.
2. Navigate to that folder.
3. From command line, run `R CMD BATCH '--args n_nodes n_edges output_file' ../../code/simulate_random_DAG.R`. Here `n_nodes` specifies the number of nodes in the network, and `n_edges` the number of edges in the network. This script will create
    a. a file `output_file.tsv` defining a network. This will have two columns: `node1` and `node2`. An edge from node X to node Y is present in the network if there's a `X Y` row in the file.
    b. a file `output_file.png` that visualized the network. 
4. From command line, run `R CMD BATCH '--args n_cores input_file' ../../code/simulate_from_DAG.R`. Here This will create the following:
    a. `dag.Rds`. This contains the DAG object created from the input_file.
