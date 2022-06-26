package tfg;

import javafx.scene.control.Alert;

/**
 * This class contains all of the alerts used.
 *
 * @author Pasku
 */
public class Alerts {

    /**
     * @deprecated
     */
    public Alerts() {
    }

    /**
     * @return An alert about a syntax error in the user query.
     */
    public static Alert alertSyntax() {
    //    Alert alert = new Alert(Alert.AlertType.ERROR, "Queries must end in point '.'");
        Alert alert = new Alert(Alert.AlertType.ERROR, "Queries must end in point '.'");
        alert.setHeaderText("Bad Syntax");
        alert.setTitle("ERROR");
        alert.setResizable(true);
        return alert;
    }

    /**
     * @return An alert about the results of the user query.
     */
    public static Alert badQuery() {
        Alert alert = new Alert(Alert.AlertType.INFORMATION, "This query was not successful.");
        alert.setHeaderText("No results");
        alert.setTitle("HEAR YE! HEAR YE!");
        alert.setResizable(true);
        return alert;
    }

    /**
     * @return An alert about an error selecting the prolog facts datablase.
     */
    public static Alert badFactsDB() {
        Alert alert = new Alert(Alert.AlertType.WARNING, "Select a valid prolog fact database to continue.");
        alert.setHeaderText("No fact database was selected");
        alert.setTitle("WARNING");
        alert.setResizable(true);
        return alert;
    }

    /**
     * Tecnically, this alert will never be triggered.
     *
     * @deprecated
     * @return An alert when trying to hide a root node.
     */
    public static Alert badHide() {
        Alert alert = new Alert(Alert.AlertType.WARNING, "Cannot hide root node.");
        alert.setHeaderText("Wrong attempt");
        alert.setTitle("ATTENTION");
        alert.setResizable(true);
        return alert;
    }

    /**
     * @return An alert about a navigation between justifications with nodes marked to hide.
     */
    public static Alert alertIllegalHide() {
        Alert alert = new Alert(Alert.AlertType.ERROR, "Your marked nodes have been reset.");
        alert.setHeaderText("Cannot hide nodes between multiple justifications");
        alert.setTitle("ERROR");
        alert.setResizable(true);
        return alert;
    }

}
