# A7_WS2018
Observer pattern

Assignment 7: MVC,Observer (Book-Manager)
----------------------------------------------

Carefully read the Observer chapter in [1].

Implement a small JavaFX [2] GUI application called Book-Manager.

Use the Observer pattern to decouple the logic of your application (Model) from the GUI (View).
In no case shall the application logic be dependent on the GUI classes.


**Book-Manager Model**  
Implement an application that manages a list of books (title, author, year, isbn, ...). 
Add support for adding, removing, editing books.

**GUI**  
Write a GUI for that application. 
The GUI shall provide a toolbar containing the buttons 'Add', 'Remove' to add and remove a book, respectively.
Also, provide a list (or a table) that displays all books.

-'Add' asks for the data of a new book and adds it (use some available dialog, see e.g. [4] -- make it simple).  
-'Remove' removes the book that is currently selected in the list/table.

Do not allow duplicates (two books with the same isbn)!  -- this is a business logic rule!

Note: The model part of your application must be able to exist without any GUI components.
Therefore, the actual list of books is part of the model, not part of a class that relates to the GUI.

The model must not depend on the view. Use the Observer pattern [1]. 
Go for the push-approach; encapsulate the broadcasted details.

Use different java packages for the model and the view.

Note that you might require exception handling in the button-handlers.

If you have troubles using JavaFx with eclipse see http://stackoverflow.com/questions/22812488/using-javafx-in-jre-8

[1] Design Patterns. Elements of Reusable Object-Oriented Software; Gamma et al.  
[2] http://docs.oracle.com/javase/8/javase-clienttechnologies.htm  
    http://www.oracle.com/technetwork/java/javase/overview/javafx-samples-2158687.html  
[3] Java Swing, Oracle: http://docs.oracle.com/javase/tutorial/uiswing/TOC.html  
[4] http://code.makery.ch/blog/javafx-dialogs-official/

```java
////////////////////////////////////////////////////////////////////////////////
/* this code might help you to get started with a javaFx application */

import javafx.application.Application;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ListView;
import javafx.scene.control.TextInputDialog;
import javafx.scene.control.ToolBar;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;

public class MainFX extends Application {

  public static void main(String[] args) {
    launch(args);   
  }

  @Override
  public void start(final Stage stage) throws Exception {
    ObservableList<Object> myList = FXCollections.observableArrayList(); //TODO adapt type to your needs
    ListView<Object> listView = new ListView<>(myList); //TODO adapt type to your needs
    
    BorderPane root = new BorderPane();
    Button cmdAdd = new Button("Add");
    cmdAdd.setOnAction(new EventHandler<ActionEvent>() {  //Option1: as anonymous class (see below)

      @Override
      public void handle(ActionEvent event) { //TODO
         //TODO 1) use a nice title and message for the dialog. 
      	 //     2) showAndWait returns an Optional<String> (handle 'Cancel' properly)
			   String input = new TextInputDialog().showAndWait().orElse(null); 
			   //here we directly add it to the list; 
			   //don't do this for the assignment. TODO: modify your 'real' model (add a new entry).
			   myList.add(input);
      }
    });

    ToolBar toolBar = new ToolBar(cmdAdd);
    root.setTop(toolBar);
    root.setCenter(listView);
    Scene scene = new Scene(root, 200, 200);
    stage.setScene(scene);
    stage.show();
  }
	
	//TODO
	//TODO
	//TODO ;-)
  
  //TODO: this class should be an Observer of your model. 
  //      in case you get a notification that the model has changed, update the list of your listView (i.e., myList)
  
  
  
  //Handlers for the Button: >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  /* Option 2: as extra class
  
  //cmdAdd.setOnAction(new AddHandler());
  
  private class AddHandler implements EventHandler<ActionEvent> {
    @Override
    public void handle(ActionEvent event) { 
    	//...
    }
  }
  */
  
  /* Option 3: lambda
    
   cmdAdd.setOnAction(e -> {
    	//System.out.println("bla");
    });
   */
}

```
