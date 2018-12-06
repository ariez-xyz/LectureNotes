import pape_sismanovic.model.Book;
import pape_sismanovic.model.Library;

public class LibraryTest {

    public static void main(String[] args) {
        Library lib = new Library();
        lib.add(new Book("978-1-4441-4712-1", 1996, "Mein Kind hat Down-Syndrom - Warum ich nicht abgetrieben habe", "Johannes Spilkas Mum"));
        lib.add(new Book("978-1-4028-9462-6", 1999, "How to be a Hurensohn", "Michael Sismanovic"));
        lib.add(new Book("420-6-1333-3337-9", 2004, "Muetter ficken - ein einfacher Guide", "David Pape aka Jesus"));
        lib.add(new Book("978-1-1923-1183-1", 2008, "Warum ich meine Entscheidung bereue", "Johannes Spilkas Mum"));
        lib.add(new Book("978-1-1491-1812-1", 2010, "Warum kann sich niemand meinen Namen merken?", "Christoph Bojko"));
        lib.add(new Book("978-1-1226-5045-1", 2018, "Enzyklopaedie nontrivialer Dinge; oder: Ein leeres Blatt Papier", "Marcell Haritopolous"));
        lib.add(new Book("978-1-1318-8452-1", 2018, "Geometrisches Rechnen ist SO GEIL!!!!", "Marcell Haritopolous"));
        lib.add(new Book("978-1-1226-8522-1", 2020, "Warum niemand Helds Wahlmodul machen sollte", "Marcell Haritopolous"));
        lib.add(new Book("978-1-1893-7534-1", 2014, "Anatomie von Beta Males an einem konkreten Beispiel", new String[]{"Dom Mazzetti", "Michael Sismanovic"}));
        lib.add(new Book("978-1-5913-8424-1", 2017, "Gay Bar Chronicles", "Matt Paul"));
        lib.add(new Book("978-1-0148-2351-1", 2018, "Wie oeffentliche Verkehrsmittel mein Studium ruiniert haben", "Filip Vecek"));
        lib.add(new Book("978-1-3812-7521-1", 2011, "Drogen, Alkohol und Nutten zur Besinnung", "Jesus aka David Pape"));
        lib.add(new Book("978-1-1893-7534-1", 2014, "TEST DUPLIKAT", ""));

        System.out.println("Bibliothek:");
        for(String isbn : lib)
            System.out.println("\t" + lib.getBook(isbn));
    }

}
