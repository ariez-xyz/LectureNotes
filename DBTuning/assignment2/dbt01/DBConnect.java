package dbt01;

import java.sql.*;
import java.io.*;

public class DBConnect {
    static int n = -1;
    static int ctr = 0;
    static long stime = 0;
    static int limit = Integer.MAX_VALUE;

    static void usage() {
        System.out.println("usage: DBConnect employees students depts");
        System.exit(1);
    }

    public static void main(String[] args) throws Exception {
        if(args.length != 3)
            usage();

        try {
            Class.forName ( "org.postgresql.Driver" );
            System.err.println("Driver found.");
        } catch ( java.lang.ClassNotFoundException e ) {
            System.err.println("PostgreSQL JDBC Driver not found ... ");
            e.printStackTrace();
            return;
        }
        
        String host = "127.0.0.1";
        String port = "5432";
        String database = "Assignment2";
        String pwd = "a1";
        String user = "a1";
        String url = "jdbc:postgresql://" + host + ":" + port + "/" + database;
        Connection con = null;
        try {
            con = DriverManager.getConnection(url, user, pwd);
            System.err.println("Connection established.");
        } catch (Exception e) {
            System.err.println("Could not establish connection.");
            e.printStackTrace();
            return;
        }
        
        //prepare db

        String[] updates = {
            "DROP TABLE IF EXISTS Auth;",
            "DROP TABLE IF EXISTS Publ;",
            "DROP TABLE IF EXISTS Students;",
            "DROP TABLE IF EXISTS Techdept;",
            "DROP TABLE IF EXISTS Persons;",
            "DROP TABLE IF EXISTS Employees;",

            "CREATE TABLE Employees(" + 
            "ssnum char(50),"+
            "name char(50)," +
            "manager char(50)," +
            "dept char(50)," +
            "salary float8," +
            "numfriends int," +
            "PRIMARY KEY(ssnum, name))",

            "CREATE TABLE Students(" + 
            "ssnum char(50),"+
            "name char(50)," +
            "course char(50)," +
            "grade int," +
            "PRIMARY KEY(ssnum, name))",

            "CREATE TABLE Techdept(" + 
            "dept char(50) PRIMARY KEY,"+
            "manager char(50)," +
            "location char(50))",
        };

        for(String qry : updates) {
            try {
                con.createStatement().executeUpdate(qry);
                ctr++;
            } catch (Exception e) {
                System.err.println("Update " + ctr + " was not successful.");
                e.printStackTrace();
            }
        }	    

        // persons
        System.out.println("copying persons...");
        con.createStatement().executeUpdate("COPY Employees (ssnum, name, manager, dept, salary, numfriends) FROM '" + System.getProperty("user.dir") + "/" + args[0] + "';");
        // students
        System.out.println("copying students...");
        con.createStatement().executeUpdate("COPY Students (ssnum, name, course, grade) FROM '" + System.getProperty("user.dir") + "/" + args[1] + "';");
        // techdept
        System.out.println("copying techdepts...");
        con.createStatement().executeUpdate("COPY Techdept (dept, manager, location) FROM '" + System.getProperty("user.dir") + "/" + args[2] + "';");

        System.out.println("creating indexes and clustering...");

        String[] moreupdates = {
            "CREATE INDEX employees_ssnum ON employees (ssnum);",
            "CREATE INDEX students_ssnum ON students (ssnum);",
            "CREATE INDEX techdept_dept ON techdept (dept);",
            "CLUSTER students USING students_ssnum;",
            "CLUSTER employees USING employees_ssnum;",
            "CLUSTER techdept USING techdept_dept;",
            "ANALYZE;",
        };

        ctr = 0;
        for(String qry : moreupdates) {
            try {
                con.createStatement().executeUpdate(qry);
                ctr++;
            } catch (Exception e) {
                System.err.println("Update " + ctr + " was not successful.");
                e.printStackTrace();
            }
        }	    
    }
}
