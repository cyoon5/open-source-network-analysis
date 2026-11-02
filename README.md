# Open Source Network Analysis

A network science project investigating collaboration patterns in open-source GitHub repositories.

## Overview

Open-source GitHub repositories rely on contributors submitting pull requests (PRs), reviewing code, and collaborating with maintainers. This project explores contributor interactions and unusual contribution patterns that may indicate suspicious activity.

## Objectives

- Collect GitHub repository, contributor, pull request, and review data.
- Construct networks representing contributor collaboration and review relationships.
- Apply network analysis techniques to identify influential contributors and unusual activity.
- Investigate potential indicators of suspicious contribution patterns.

## Planned Analysis

- Degree Centrality
- Betweenness Centrality
- Eigenvector Centrality
- Katz Centrality
- Network Visualization

## Technologies

- R
- igraph
- GitHub API

## Group Members

- Chanyee Yoon
- Carter Ludwig

## High level code design

5-6 files: 
Main	
Collection
This file will focus on functions that send sql queries to the github api and graphql servers.
data_collection.R	Retrieve repository, contributor, PR and review data from GitHub
Constructing graphs
This file will focus on functions that output graphs such as the bipartite network and the multi layer network as well as a way to do a one mode projection given a target column
data_processing.R	Clean data and prepare contributor relationships
Metrics
Functions for evaluating and retrieving valuable information from the network such as various centralities and other metrics.
network_analysis.R	Construct networks and calculate centrality measures 
ML?
If have time
Will be used to construct and train ML models that can assist with predicting if a PR is AI or not based off the metrics and the structure of the graphs.
Plotting
Functions that will be used to construct plots and graphs and store inside the data directory.


