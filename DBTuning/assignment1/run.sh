#!/bin/bash
export CLASSPATH=/Users/dpape/Documents/Uni/DBTuning/assignment1/postgresql-42.2.5.jar:/Users/dpape/Documents/Uni/DBTuning/assignment1/
javac dbt01/DBConnect.java && java dbt01.DBConnect $1 $2
export CLASSPATH=.
