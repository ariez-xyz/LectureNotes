package dbt01;

import java.sql.*;
import java.io.*;

public class DBConnect {
    
    public static void main(String[] args) throws Exception {
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
        String database = "assignment1";
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

            "CREATE TABLE Auth(" + 
            "name char(49),"+
            "pubId char(129))",

            "CREATE TABLE Publ(" +
            "pubId char(129)," +
            "type char(13)," +
            "title char(700)," +
            "booktitle char(132)," +
            "year char(4)," +
            "publisher char(196))"
        };

        int ctr = 0;

        try {
            for(String qry : updates) {
                con.createStatement().executeUpdate(qry);
                ctr++;
            }	    
            System.out.println("Updates sucessful.");
        } catch (Exception e) {
            System.err.println("Update " + ctr + " was not successful.");
            e.printStackTrace();
        }

        //read auth

        long time = System.currentTimeMillis();
        try (BufferedReader br = new BufferedReader(new FileReader(new File("./auth.tsv")))) {
            String line;
            ctr = 0;
            while((line = br.readLine()) != null) {
                String[] split = line.split("\t");
                int test = split[0].indexOf('\'');
                if(test >= 0)
                    split[0] = split[0].substring(0, test) + split[0].substring(test + 1, split[0].length());
                con.createStatement().executeUpdate(String.format("INSERT INTO Auth (name, pubId) VALUES ('%s', '%s')", split[0], split[1]));
                System.err.print("n = " + ctr++ + ", t = "+ (System.currentTimeMillis() - time) +"\r");
            }
        }
    }
}
