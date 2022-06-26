package tfg;

import java.io.IOException;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

/**
 * This is the main class, which launches the main controller and deals with
 * main handlers such as timers and key combinations.
 *
 * @author Pasku
 */
public class Main extends Application {

    /**
     * Shows if the key is pressed or not.
     */
    private Boolean ctrlPressed = false, plusPressed = false, minusPressed = false;

    /**
     * To control the Zoom we can either use the slider or press 'Ctrl +' to
     * Zoom In, or 'Ctrl -' to Zoom Out.
     *
     * @param primaryStage The main stage.
     * @throws IOException If the justification tree can not be loaded.
     */
    @Override
    public void start(Stage primaryStage) throws IOException {
        FXMLLoader loader = new FXMLLoader(getClass().getResource("JustificationTree.fxml"));
        Parent root = loader.load();

        primaryStage.getIcons().add(new Image(Main.class.getResourceAsStream("/images/prologIcon.png")));
        primaryStage.setTitle("PROLOG PROGRAM TREE JUSTIFICATOR");
        Scene scene = new Scene(root);
        primaryStage.setScene(scene);
        primaryStage.setResizable(false);
        primaryStage.show();
        JustificationTreeController controller = loader.getController();

        scene.addEventFilter(KeyEvent.KEY_PRESSED, (KeyEvent event) -> {
            if (event.getCode() == KeyCode.CONTROL || event.getCode() == KeyCode.ADD || event.getCode() == KeyCode.SUBTRACT) {
                switch (event.getCode()) {
                    case CONTROL:
                        ctrlPressed = true;
                        break;
                    case ADD:
                        plusPressed = true;
                        break;
                    case SUBTRACT:
                        minusPressed = true;
                        break;
                    default:
                        break;
                }
                controller.initData(ctrlPressed, plusPressed, minusPressed);
                event.consume();
            }
        });

        scene.addEventFilter(KeyEvent.KEY_RELEASED, (KeyEvent event) -> {
            if (event.getCode() == KeyCode.CONTROL || event.getCode() == KeyCode.ADD || event.getCode() == KeyCode.SUBTRACT) {
                switch (event.getCode()) {
                    case CONTROL:
                        ctrlPressed = false;
                        break;
                    case ADD:
                        plusPressed = false;
                        break;
                    case SUBTRACT:
                        minusPressed = false;
                        break;
                    default:
                        break;
                }
                controller.initData(ctrlPressed, plusPressed, minusPressed);
            }
            event.consume();
        });
    }

    /**
     * Stops the timer associated with the Prolog Curious Facts running in
     * background.
     */
    @Override
    public void stop() {
        System.exit(0);
    }

    /**
     * Launches the program itself (calls the{@code 'start}' method).
     *
     * @param args The program arguments.
     */
    public static void main(String[] args) {
        launch(args);
    }

}
