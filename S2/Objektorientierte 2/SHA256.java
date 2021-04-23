//David Pape 01634454
//Johannes Spilka 11724817
//Filip Vecek 11700962

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;

public class SHA256 {
    String data;
    String hash;

    public SHA256(String data) {
        this.data = data;

        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] encodedhash = digest.digest(
                    this.data.getBytes(StandardCharsets.UTF_8));
            hash = bytesToHex(encodedhash);
        } catch (Exception e) {}

    }

    private String bytesToHex(byte[] hash) {
        StringBuffer hexString = new StringBuffer();
        for (int i = 0; i < hash.length; i++) {
            String hex = Integer.toHexString(0xff & hash[i]);
            if(hex.length() == 1) hexString.append('0');
            hexString.append(hex);
        }
        return hexString.toString();
    }

    public String toString() {
        return hash;
    }
}
