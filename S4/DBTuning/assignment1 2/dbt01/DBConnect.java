package dbt01;

import java.sql.*;
import java.io.*;

public class DBConnect {
    static int n = -1;
    static int ctr = 0;
    static long stime = 0;
    static int limit = Integer.MAX_VALUE;

    static void usage() {
        System.out.println("usage: DBConnect strategy [ limit ]");
        System.out.println("available strategies:");
        System.out.println("\t<1> naive loop");
        System.out.println("\t<2> naive loop without autocommit");
        System.out.println("\t<3> COPY statement (limit unsupported)");
        System.exit(1);
    }

    public static void main(String[] args) throws Exception {
        if(args.length != 1 && args.length != 2)
            usage();

        int strategy = Integer.parseInt(args[0]);
        if(args.length > 1)
            limit = Integer.parseInt(args[1]);

        //print stats on ctrl c
        Runtime.getRuntime().addShutdownHook(new Thread() {
            public void run() {
                System.out.println(String.format("\nadded ~%d entries in %fs", n, (double) (System.currentTimeMillis() - stime) / 1000));
            }
        });
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

        try {
            for(String qry : updates) {
                con.createStatement().executeUpdate(qry);
                ctr++;
            }	    
        } catch (Exception e) {
            System.err.println("Update " + ctr + " was not successful.");
            e.printStackTrace();
        }

        //read auth
        switch(strategy) {
            case 1: naive(con);
                    break;
            case 2: noautocommit(con);
                    break;
            case 3: copy(con);
                    break;
            default: System.out.println("[ERROR] unknown strategy selected");
        }
    }

    static void naive(Connection con) throws Exception {
        stime = System.currentTimeMillis();
        try (BufferedReader br = new BufferedReader(new FileReader(new File("./auth.tsv")))) {
            String line;
            ctr = 0;
            System.err.println("reading auth...");
            while((line = br.readLine()) != null && n < limit) {
                String[] split = line.split("\t");
                int test = split[0].indexOf('\'');

                // deal with Patrick E. O'Neil 
                if(test >= 0)
                    split[0] = split[0].substring(0, test) + '\'' + split[0].substring(test);

                con.createStatement().executeUpdate(String.format("INSERT INTO Auth (name, pubId) VALUES ('%s', '%s')", split[0], split[1]));
                n++;
            }
        }
    }

    static void noautocommit(Connection con) throws Exception {
        con.createStatement().executeUpdate("BEGIN;");
        naive(con);
        con.createStatement().executeUpdate("COMMIT;");
    }

    static void copy(Connection con) throws Exception {
        stime = System.currentTimeMillis();
        con.createStatement().executeUpdate("COPY Auth (name, pubId) FROM '" + System.getProperty("user.dir") + "/auth.tsv';");
    }
}
