# Create and Simulate From Directed Graph

1. Create subdirectory under `data/` where you want to put the data.
2. Navigate to that folder.
3. From command line, run `R CMD BATCH '--args n_nodes n_edges output_file' ../../code/simulate_random_DAG.R`. Here `n_nodes` specifies the number of nodes in the network, and `n_edges` the number of edges in the network. This script will create
    a. a file `output_file.tsv` defining a network. This will have two columns: `node1` and `node2`. An edge from node X to node Y is present in the network if there's a `X Y` row in the file.
    b. a file `output_file.png` that visualized the network. 
4. From command line, run `R CMD BATCH '--args n_cores input_file' ../../code/simulate_from_DAG.R`. Here `n_cores` is the number of cores to use (if in doubt, set to 1), and `input_file` is the file specifying the network (probably `output_file.tsv` from step 3). This will create the following:
    a. `dag.Rds`. This contains the DAG object created.
    b. `DAG_figure.png`. This is a graphical representation of the DAG object created. Should be the exact same as `output_file.png`
    c. `sim_data.csv`. A .csv file with the simulated time series data. 
5. From command line, run `R CMD BATCH '--args sim_data.csv' ../../code/reshape_sim_data.R`. This will create
    a. three folders: `raw_data/`, `normalized_data/`, and `permuted_data/` 
    b. in each of the three folders, there will be on text file per sample with the time series data for that sample. There will also be a `_reps.txt` file. This is simply a list of filenames which is needed for `BETS` to run. 
    c. a file `list_of_genenames.txt`. Simply a list of genenames. Again, needed for `BETS`.

