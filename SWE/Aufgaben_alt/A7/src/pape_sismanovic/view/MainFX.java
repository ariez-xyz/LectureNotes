package pape_sismanovic.view;

import javafx.application.Application;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;
import pape_sismanovic.exceptions.InputFormatException;

import pape_sismanovic.interfaces.Observer;
import pape_sismanovic.model.Book;
import pape_sismanovic.model.Event;
import pape_sismanovic.model.Library;

import java.util.Arrays;
import java.util.HashMap;

/**
 * Loosely coupled graphical interface for pape_sismanovic.model.Library.
 */
public class MainFX extends Application implements Observer {

    private Library lib;
    private ObservableList<String> booksList;
    private ListView<String> booksView;
    // Maps strings in booksList to ISBN. Avoids having to parse said strings for ISBNs for deletion etc.
    private HashMap<String, String> isbnMap;

    @Override
    public void update(Event e) {
        switch (e.type()) {
            case ADD:
                String newBookString = prettyPrintBook(lib.getBook(e.affectedIsbn()));
                isbnMap.put(newBookString, e.affectedIsbn());
                booksList.add(newBookString);
                break;
            case DELETE:
                for (String b1 : booksList)
                    if (isbnMap.get(b1).equals(e.affectedIsbn())) {
                        booksList.remove(b1);
                        return;
                    }
            case EDIT:
                for (String b2 : booksList)
                    if (isbnMap.get(b2).equals(e.affectedIsbn())) {
                        booksList.remove(b2);
                        break;
                    }
                booksList.add(prettyPrintBook(lib.getBook(e.newIsbn())));
        }
    }

    private String prettyPrintBook(Book b) {
        StringBuilder sb = new StringBuilder();

        for (String author : b.getAuthors()) {
            sb.append(author).append(", ");
        }

        return String.format("'%s' (%s) - %s [%s]", b.getTitle(), b.getYear(), sb.delete(sb.length() - 2, sb.length()).toString(), b.getIsbn());
    }


    /**
     * Attempt to parse a string representation of a book
     * Valid format in pseudo-EBNF:
     * book = title, { author }, isbn, year
     * @param encodedBook String representing the book
     * @throws InputFormatException If input string does not follow format
     */
    private Book parse(String encodedBook) throws InputFormatException {
        String[] items = encodedBook.split(",");

        if (items.length < 4)
            throw new InputFormatException("Input does not fully specify a book. Required: TITLE, AUTHOR(S), ISBN, YEAR");

        for (int i = 0; i < items.length; i++) // get rid of whitespaces
            items[i] = items[i].trim();

        try {
            String title = items[0];
            String[] authors = Arrays.copyOfRange(items, 1, items.length - 2);

            // Check if ISBN has characters besides digits and '-', return null if true
            String isbn = items[items.length - 2];
            for(char c : isbn.toCharArray())
                if (!Character.isDigit(c) && c != '-')
                    throw new InputFormatException("Bad ISBN: " + isbn + "\nISBN can only contain numbers and dashes");

            int year = Integer.parseInt(items[items.length - 1]);

            return new Book(isbn, year, title, authors);
        } catch (NumberFormatException e) { // invalid release year
            throw new InputFormatException("Invalid release year: " + items[items.length - 1] + "\nRelease year must be a number");
        }
    }

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(final Stage stage) {
        booksList = FXCollections.observableArrayList();
        booksView = new ListView<>(booksList);
        isbnMap = new HashMap<>();

        lib = new Library();
        lib.registerObserver(this);

        BorderPane root = new BorderPane();
        Button cmdAdd = new Button("Add");
        Button cmdDel = new Button("Delete");

        cmdAdd.setOnAction(event -> {
            // Input popup
            TextInputDialog dialog = new TextInputDialog("Design Patterns: Elements of Reusable Object-Oriented Software, Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides, 978-0201633610, 1994");
            dialog.setHeaderText("Add book");
            dialog.setContentText("Format: TITLE, AUTHOR(S), ISBN, YEAR");
            String in = dialog.showAndWait().orElse(null);

            if(in == null) // cancelled
                return;

            try {
                lib.add(parse(in));
            } catch (InputFormatException e) {
                // Bad input popup
                Alert errorAlert = new Alert(Alert.AlertType.ERROR);
                errorAlert.setHeaderText("Invalid input");
                errorAlert.setContentText(e.getMessage());
                errorAlert.showAndWait();
            }
        });

        cmdDel.setOnAction(event -> {
            int selectedIndex = booksView.getSelectionModel().getSelectedIndex();
            if (selectedIndex >= 0) {
                // Confirmation popup
                Alert alert = new Alert(Alert.AlertType.WARNING, null, ButtonType.YES, ButtonType.NO);
                alert.setHeaderText("Delete selected item?");
                alert.showAndWait().ifPresent(response -> {
                    if (response == ButtonType.YES)
                        lib.delete(isbnMap.get(booksList.get(selectedIndex)));
                });
            }
            else {
                Alert errorAlert = new Alert(Alert.AlertType.ERROR, "Please select an item in the list");
                errorAlert.setHeaderText("Cannot delete");
                errorAlert.showAndWait();
            }
        });

        ToolBar toolBar = new ToolBar(cmdAdd, cmdDel);
        root.setTop(toolBar);
        root.setCenter(booksView);
        Scene scene = new Scene(root, 500, 500);
        stage.setScene(scene);
        stage.show();
    }
}
