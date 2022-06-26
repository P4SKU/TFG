package tfg;

import java.net.URL;
import java.util.Map;
import java.util.ResourceBundle;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Label;
import javafx.scene.control.TreeView;
import javafx.scene.layout.AnchorPane;
import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ThreadLocalRandom;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Side;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.MultipleSelectionModel;
import javafx.scene.control.RadioButton;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.Slider;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleGroup;
import javafx.scene.control.TreeItem;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.scene.text.Font;
import javafx.scene.text.FontPosture;
import javafx.scene.text.FontWeight;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;
import javafx.stage.FileChooser;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.util.StringConverter;

/**
 * FXML Controller class
 *<br><br><strong>
     * TODO Try catches</strong>
 *
 * @author Pasku
 */
public class JustificationTreeController implements Initializable {
    /**
     * FileChooser to select the facts database.
     */
    FileChooser fileChooser = new FileChooser();
        /**
     * The selection model to allow selecting multiple TreeItems.
     */
    MultipleSelectionModel selectionModel;
            /**
     * Items of a justification selected by the user to be hidden.
     */
    ObservableList<Integer> atomsToHide;
                /**
     * The main stage.
     */
    private Stage stage;
            /**
     * The name of the facts database.
     */
    String factDB = "";
                /**
     * The user query to be executed.
     */
     String userQuery = "";
                 /**
     * All justifications splitted and stored.
     */
    String[] solutions;
                /**
     * The randomly rotative curious prolog facts stored.
     */
    String[] prologFacts;

    /**
    * The justification index.
    */
    int tabPage = 0;
     /**
    * The font size for all TreeItems.
    */
    int fontSize = 12;
     /**
    * The minimum level in each justification.
    */
            int minLevel = 0;
             /**
    * Number of nodes in previous justifications.
    */
            int aggregateNodes;
             /**
    * The current expanded level in each justification.
    */
    int[] currentLevel;
     /**
    * The maximum level in each justification.
    */
     int[] maxLevel;
      /**
    * Number of nodes per justification.
    */
      int[] nodesInTab;
    /**
    * Number of childs per node.
    */
      int[] childs;
            /**
    * Checks wheter it is the first execution (build) or not (rebuild).
    */
    boolean firstExec = true;
          /**
    * Checks if the justification representation is atomic.
    */
    boolean showAtoms = true;
          /**
    * Checks if the Radio Button (option) was selected.
    */
    boolean contracted = false, deployed = false;
        /**
     * Shows if the key is pressed or not.
     */
    boolean ctrlPressed = false, plusPressed = false, minusPressed = false;
        /**
     * Stores the splitted solutions indexes.
     */
    Tab[] solTabs;
    
    
    List<Map<Integer, String>> arrayLevelsAtom;
    List<Map<Integer, String>> arrayLevelsTextual;
    List<Map<Integer, String>> arbolAtoms = new ArrayList<>();
    List<Map<Integer, String>> nodesToDelete = new ArrayList<>();
    List<Map<Integer, String>> arbolTextual = new ArrayList<>();
    
    List<Map<Integer, TreeItem<CheckBox>>> nodosAtomList, nodosTextualList = new ArrayList<>();
    List<Map<Integer, TreeItem<CheckBox>>> deletedNodes = new ArrayList<>();
    
       /**
     * Each justification TreeView root item stored.
     */
    TreeView<CheckBox>[] treeRoot;

    /**
     * The Justification Tree (and representation) in v1.0.
     *
     * @deprecated
     */
    TreeItem<CheckBox>[] nodosAtom = null, nodosTextual;

    @FXML
    private AnchorPane root;
    @FXML
    private ImageView imggg;
    @FXML
    private TabPane tabPane;
    @FXML
    private Button buttonLoadBH;
    @FXML
    private Label cargarBH;
    @FXML
    private Button buttonExecute;
    @FXML
    private TextField queryPrompt;
    @FXML
    private TextFlow datosCuriosos;
    @FXML
    private Label sabiasque;
    @FXML
    private Label cargueParaEmpezar;
    @FXML
    private Label cargarBH1;
    @FXML
    private RadioButton radioAtom;
    @FXML
    private ToggleGroup grupo1;
    @FXML
    private RadioButton radioPhrase;
    @FXML
    private Button buttonHide;
    @FXML
    private RadioButton radioContract;
    @FXML
    private RadioButton radioDeploy;
    @FXML
    private ToggleGroup grupo2;
    @FXML
    private Label justificationTree;
    @FXML
    private Slider scrollZoom;
    @FXML
    private ImageView openHiddenNodesImg;

    /**
     * Loads the interpreter 'tracer.pl', initializes the prolog curious facts and manages the zoom level.
     * @param url URL
     * @param rb ResourceBundle
     */
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        try {


            Query q1 = new Query("consult", new Term[]{new Atom("../tracer/tracer.pl")});
            q1.hasSolution();
     

            showPrologFacts();

            buttonExecute.setDisable(true);
            scrollZoom.setDisable(true);
            openHiddenNodesImg.setImage(null);
            justificationTree.setText("");

            scrollZoom.setLabelFormatter(new StringConverter<Double>() {
                @Override
                public String toString(Double n) {
                    String val = "";
                    if (n < 15) {
                        val = "MIN";
                    }
                    if (n == 24) {
                        val = "MED";
                    }
                    if (n == 36) {
                        val = "HIGH";
                    }
                    return val;
                }

                @Override
                public Double fromString(String s) {
                    switch (s) {
                        case "MIN":
                            return 12d;
                        case "MED":
                            return 19d;
                        case "HIGH":
                            return 26d;
                        default:
                            return 19d;
                    }
                }
            });

            /* Manages the zoom level*/
            scrollZoom.valueProperty().addListener((observable, oldValue, newValue) -> {
                int zoomValue = (int) scrollZoom.getValue();
                if (zoomValue > 30 || zoomValue < 18 || zoomValue > 0) {
                    applyZoom();
                }
            });

        } catch (Exception e) {
            System.out.println("[Error]: " + e.getMessage() + " at initialize()");
        }
    }

    /**
     * Loads the facts database from File System.
     * 
     * @param event When clicking the 'Load' button.
     */
    @FXML
    private void loadFactsDB(ActionEvent event) {
        try {
            fileChooser.setInitialDirectory(new File(System.getProperty("user.dir")));
            File file = fileChooser.showOpenDialog(stage);
            if (file != null) {
                factDB = file.toString();
                factDB = factDB.replace("\\", "/"); // Prolog uses normal backlash for routes
                factDB = factDB.substring(factDB.lastIndexOf("/") + 1);
                buttonExecute.setDisable(false);
                buttonLoadBH.getStyleClass().clear();
                buttonLoadBH.getStyleClass().add("mainButtonOkay");
                if (!factDB.endsWith(".pl")) {
                    buttonExecute.setDisable(true);
                    buttonLoadBH.getStyleClass().clear();
                    buttonLoadBH.getStyleClass().add("mainButton");
                    scrollZoom.setDisable(true);
                    badFactsDB();
                }
            }
        } catch (Exception e) {
            System.out.println("[Error]: " + e.getMessage() + " at loadFactsDB");
        }
    }

    /**
     * Shows the curious Prolog Facts randomly each 15 seconds.
     */
    private void showPrologFacts() {
        try {
            Timer timer = new Timer();
            timer.scheduleAtFixedRate(new TimerTask() {
                @Override
                public void run() {
                    datosCuriosos.setStyle("-fx-font-size: 15px;");
                    initializePrologCuriousData();
                    Platform.runLater(() -> {
                        int prologFactIndex = ThreadLocalRandom.current().nextInt(0, 26); // We have just 27 facts
                        datosCuriosos.getChildren().removeAll(datosCuriosos.getChildren()); // Removing the previous fact
                        Text dato = new Text(prologFacts[prologFactIndex]);
                        dato.setFont(Font.font("Verdana", FontPosture.ITALIC, 15));
                        datosCuriosos.getChildren().add(dato); // Adding the new fact
                    });
                }
            }, 3500, 20000); // 3500 -> 3.5 Seconds until the first fact appears, 20000 -> showing new facts each 20 secs
        } catch (Exception e) {
            System.out.println("[Error]: " + e.getMessage() + " at showPrologFacts");
        }
    }

    /**
     * Prepares the array containing the Prolog Facts.
     */
    private void initializePrologCuriousData() {
        try {
            prologFacts = new String[27];
            int i = 0;
            File prologData;
            prologData = new File(System.getProperty("user.dir") + File.separator + "prologFacts.txt");
            Scanner scan = new Scanner(prologData);
            while (scan.hasNextLine()) {
                String prologFact = scan.nextLine();
                prologFacts[i++] = prologFact;
            }
        } catch (FileNotFoundException e) {
            System.out.println("[Error]: " + e.getMessage() + " at initilizePrologCuriousData");
        }
    }

    /**
     * Obtains the user Query and constructs the TreeView Justification.
     * 
     * @param event When clicking the 'Execute' button.
     */
    @FXML
    private void fireQuery(ActionEvent event) {
        try {
            firstExec = true;
            radioAtom.setSelected(false);
            radioPhrase.setSelected(false);
            atomsToHide = null;

            if(showAtoms) radioAtom.setSelected(true); else radioPhrase.setSelected(true);
            scrollZoom.setDisable(false);
            radioContract.setSelected(false);
//            radioPhrase.setSelected(false);
//            radioAtom.setSelected(true);
           radioDeploy.setSelected(false);
            openHiddenNodesImg.setImage(new Image("/images/Back.png"));
            justificationTree.setText("Justification Tree");
            if (!checkQuerySyntax()) {
                buttonExecute.getStyleClass().clear();
                buttonExecute.getStyleClass().add("mainButton");
                alertSyntax();
            } else {
                buttonExecute.getStyleClass().clear();
                buttonExecute.getStyleClass().add("mainButtonOkay");
                userQuery = queryPrompt.getText().substring(0, queryPrompt.getText().length() - 1);
                userQuery = userQuery.replace("_", "X");
                // System.out.println(userQuery);
                if (consultBH()) {
                    tabPane.getTabs().removeAll(tabPane.getTabs());
                    buildTreeView(fontSize);
                    cargueParaEmpezar.setText("");
                } 
            }
        } catch (InputMismatchException e) {
            System.out.println("[Error]: " + e.getMessage() + " at getConsulta()");
        }
    }

    /**
     * Parses the Syntax of the user query. Just checking that ends in point '.'
     * by now.
     *
     * @return If the query syntax is correct or not.
     */
    private boolean checkQuerySyntax() {
        int lastPoint = queryPrompt.getText().indexOf(".");
        return (lastPoint == queryPrompt.getText().length() - 1); // Queries must end in point.

        /* Deprecated */
//        int openingParenthesis = queryPrompt.getText().indexOf("("); 
//        if (openingParenthesis <= 0) {                  // -1 shows that there is not such a character
//            return false;                          // if there is a '(' we also need some text before it
//        }
//        int closingParenthesis = queryPrompt.getText().indexOf(")");
//        if (closingParenthesis < 0 || (closingParenthesis != queryPrompt.getText().length() - 2)) {
//            return false; // if there is ')' it must be the penultimate
//        }
//        int comma = queryPrompt.getText().indexOf(",");
//        if (comma < openingParenthesis || comma < 0) {
//            return false; // Comma should exist and be inside both parenthesis.
//        }
//        boolean isVariable = false;
//        String[] params = new String[2];
//        params[0] = queryPrompt.getText().substring(openingParenthesis + 1, comma);
//        params[1] = queryPrompt.getText().substring(comma + 1, closingParenthesis);
//        char[] param1 = params[0].toCharArray();
//        char[] param2 = params[1].toCharArray();
//        /* solo recorro param1 ,y si esta en param2??? */
//        for (int r = 0; r < param1.length && !isVariable; r++) {
//            isVariable = Character.isUpperCase(param1[r]) || param1[r] == '_';       // Either we have a variable X or a Wildcard '_'
//            if (isVariable && param1.length == 1 && param2.length == 1 && Character.isUpperCase(param2[0])) {
//                return false;                           // We would have 2 variables.
//            }
//            if (isVariable) {
//                userVariable = Character.compare('_', param1[r]) == 0 ? "X" : param1[r] + ""; // Replacing the Wildcard by another variable as it is not recognised by the library. Doesn't affect the performance.
//            }
//
//        }
//
//        if (!isVariable) {
//            return false;
//        }
    }

    /**
     * An alert about a syntax error in the user query.
     */
    private void alertSyntax() {
        Alert alert = Alerts.alertSyntax();
        alert.initOwner(buttonExecute.getScene().getWindow());
        alert.showAndWait();
    }

    /**
     * An alert about the results of the user query.
     */
    private void badQuery() {
        if (firstExec) {
            firstExec = false;
            Alert alert = Alerts.badQuery();
            alert.initOwner(buttonExecute.getScene().getWindow());
            alert.showAndWait();
        }
    }

    /**
     * An alert about an error selecting the prolog facts datablase.
     */
    private void badFactsDB() {
        Alert alert = Alerts.badFactsDB();
        alert.initOwner(buttonExecute.getScene().getWindow());
        alert.showAndWait();
    }

    /**
     * An alert when trying to hide a root node.
     * Tecnically, this alert will never be triggered.
     *
     * @deprecated
     */
    private void badHide() {
        Alert alert = Alerts.badHide();
        alert.initOwner(buttonExecute.getScene().getWindow());
        alert.showAndWait();
    }

    /**
     * An alert about a navigation between justifications with nodes
     * marked to hide.
     */
    private void alertIllegalHide() {
        nodesToDelete.clear();

        for (int i = 0; i < nodosAtomList.size(); i++) {
            nodosAtomList.get(i).values().iterator().next().getValue().selectedProperty().set(false);
            nodosTextualList.get(i).values().iterator().next().getValue().selectedProperty().set(false);
        }
        Alert alert = Alerts.alertIllegalHide();
        alert.initOwner(buttonExecute.getScene().getWindow());
        alert.showAndWait();
    }

    /**
     * Queries the Facts Database.
     *
     * @return Either the query had success or not.
     */
    private boolean consultBH() {
        try {
            Query q2 = new Query("load('" + factDB + "').");    // Loads the user Facts Database to my 'tracer.pl'.
            q2.hasSolution();
            Query q3 = new Query("run([" + userQuery + "]).");       // Queries the Facts Database.
            q3.hasSolution();
            Map<String, Term> binding = q3.next();
            //   Term X = (Term) binding.get(userVariable);         // We can specify and get a user variable
            Term X = (Term) binding.get("");
            return true;           // There is result
        } catch (Exception e) {
            System.out.println("[Error]: " + e.getMessage() + " at consultBH");
            return false;
        }
    }

    /**
     * Finds the inmediate parent node, by looking the previous first node with
     * a higher layer level.
     *
     * @param index The position in array of the node.
     * @param myLevel The level of the node.
     * @return The index of the parent node, or -1 if there is none (i.e. root).
     * @deprecated
     */
    private int findParentNode(int index, int myLevel) {
        for (int w = index; w >= 0; w--) {
            if (showAtoms) {
                int level = (int) arbolAtoms.get(w).keySet().iterator().next();
                if (level == myLevel - 1) {
                    return w;
                }
            } else {
                int nivel = (int) arbolTextual.get(w).keySet().iterator().next();
                if (nivel == myLevel - 1) {
                    return w;
                }
            }
        }
        return -1;
    }

    /**
     * Finds the inmediate parent node, by looking the previous first node with
     * a higher layer level.
     *
     * @param myPos The position in array of the node.
     * @param myLevel The level of the node.
     * @return The index of the parent node, or -1 if there is none (i.e. root).
     */
    private int findParentNodeList(int myPos, int myLevel) {
        for (int w = myPos; w >= 0; w--) {
            if (showAtoms) {
                int level = (int) nodosAtomList.get(w).keySet().iterator().next();
                if (level == myLevel - 1) {
                    return w;
                }
            } else {
                int nivel = (int) nodosAtomList.get(w).keySet().iterator().next();
                if (nivel == myLevel - 1) {
                    return w;
                }
            }
        }
        return -1;
    }
    
        /**
     * Finds how many sibling nodes are there.
     *
     * @param fatherNode The index of the father node.
     * @return The number of childs.
     */
    private int getChilds(int fatherNode) {
        return childs[fatherNode];
    }

    /**
     * Finds the inmediate parent node, by looking the previous first node with
     * a higher layer level.
     *
     * @param myPos The position in array of the node.
     * @param myLevel The level of the node.
     * @return The index of the parent node, or -1 if there is none (i.e. root).
     * @deprecated
     */
    private int findParentTextualList(int myPos, int myLevel) {
        for (int w = myPos; w >= 0; w--) {
            if (showAtoms) {
                int level = (int) nodosTextualList.get(w).keySet().iterator().next();
                if (level == myLevel - 1) {
                    return w;
                }
            } else {
                int nivel = (int) nodosTextualList.get(w).keySet().iterator().next();
                if (nivel == myLevel - 1) {
                    return w;
                }
            }
        }
        return -1;
    }

    /**
     * Clips each justification.
     *
     * @return A list containing the nodes of a single justification.
     * @param rawSolution The entire String.
     * @param showingAtoms Either the justification tree representation must be
     * atomic or textual.
     */
    private List<Map<Integer, String>> solutionClipper(String rawSolution, boolean showingAtoms) {
        String[] auxSolutions = rawSolution.split(";");
        arrayLevelsAtom = new ArrayList<>();
        arrayLevelsTextual = new ArrayList<>();
        for (String solution : auxSolutions) {
            int index = solution.indexOf(",");
            if (index == -1) { // If there are no solutions finish here.
                badQuery();
                buttonExecute.getStyleClass().clear();
                buttonExecute.getStyleClass().add("mainButton");
                Map<Integer, String> nodoNull = new HashMap<>();
                nodoNull.put(-1, "0");
                arrayLevelsAtom.add(nodoNull);
                return arrayLevelsAtom;
            }                   // If there are solutions, list them.
            String nivel = solution.substring(0, index);
            int lastComma = solution.lastIndexOf(",");
            String atomo = solution.substring(solution.indexOf(",") + 1, lastComma);
            String textual = solution.substring(lastComma + 2, solution.length() - 1);
            Map<Integer, String> nodoAtom = new HashMap<>();
            Map<Integer, String> nodoTextual = new HashMap<>();
            nodoAtom.put(Integer.parseInt(nivel), atomo);
            nodoTextual.put(Integer.parseInt(nivel), textual);
            arrayLevelsAtom.add(nodoAtom);
            arrayLevelsTextual.add(nodoTextual);
        }
        return showingAtoms ? arrayLevelsAtom : arrayLevelsTextual;
    }

    /**
     * Builds the Tree View justification.
     *
     * @param fontSize The font size (or zoom option).
     */
    private void buildTreeView(int fontSize) {
        deletedNodes.clear();
        FXMLLoader loader = new FXMLLoader(getClass().getResource("HiddenNodes.fxml"));
        try {
            Parent rootScene = loader.load();
        } catch (IOException e) {
            System.out.println("[Error] " + e.getMessage() + " at buildTreeView");
        }

        HiddenNodesController hiddenNodesController = loader.<HiddenNodesController>getController();
        hiddenNodesController.initData(deletedNodes);
        nodosAtomList = new ArrayList<>();
        nodosTextualList = new ArrayList<>();

        /* Reading the temp file created ealier when using my 'tracer.pl' */
        File file;
        file = new File(System.getProperty("user.dir") + File.separator + "temp.txt");

        try {
            Scanner sc = new Scanner(file);
            String i = sc.nextLine();
            i = i.replace("[[", "");
            i = i.replace("]]", "");
            i = i.replace("not_call", "");
            i = i.replace("call", "");
            i = i.replace("],[", ";");
            i = i.trim();

            solutions = i.split(";");
            solTabs = new Tab[solutions.length];
            nodesInTab = new int[solutions.length];
            treeRoot = new TreeView[solutions.length];

            Map<Integer, TreeItem<CheckBox>> thisNode, thisNodeTextual;
            TreeItem<CheckBox> thisItem, thisItemTextual;
            maxLevel = new int[solutions.length];
            currentLevel = new int[solutions.length];
//             childs = new int[solutions.length];
//                for (int s = 0; s < solutions.length; s++) {
//                    childs[s]=0;
//                }
            for (int s = 0; s < solutions.length; s++) {
                currentLevel[s] = 1;
                treeRoot[s] = new TreeView<>();
               

                solTabs[s] = new Tab("Justification " + (s + 1));
                solTabs[s].setClosable(false);
                tabPane.getTabs().add(solTabs[s]);

                tabPane.setSide(Side.BOTTOM);
                tabPane.getSelectionModel().select(tabPage);

                String singleSol = solutions[s];
                singleSol = singleSol.substring(1, singleSol.length() - 1);
                singleSol = singleSol.replace("),(", ";");
                arbolAtoms = solutionClipper(singleSol, true);
                nodesInTab[s] = arbolAtoms.size();
                arbolTextual = solutionClipper(singleSol, false);
                 childs = new int[ arbolAtoms.size()];


                if (!((int) arbolAtoms.get(0).keySet().iterator().next() == -1)) {  // If there are solutions...
                    TreeItem<CheckBox> rootItemAtom = null;
                    TreeItem<CheckBox> rootItemTextual = null;
                    
                    
                    int nodeIndex, nodeIndexTextual;
                    for (int w = 0; w < arbolAtoms.size(); w++) {
                        int level = (int) arbolAtoms.get(w).keySet().iterator().next();
                        thisNode = new HashMap<>();
                        thisNodeTextual = new HashMap<>();
                        if (level == 0) {

                            String previousText =  arbolTextual.get(w).values().iterator().next();
                            CheckBox rootNode = new CheckBox(arbolAtoms.get(w).values().iterator().next());
                            CheckBox rootNodeTextual = new CheckBox(previousText);
                            thisItem = new TreeItem<>(rootNode);
                            thisItemTextual = new TreeItem<>(rootNodeTextual);
                            thisItem.getValue().setId(s + "-" + w);
                            thisItemTextual.getValue().setId(s + "-" + w);
                            String previousId = thisItemTextual.getValue().getId();

                            //IMPIDE ELIMINAR EL ROOT NODE
                            rootNode.selectedProperty().addListener((ObservableValue<? extends Boolean> ov, Boolean old_val, Boolean new_val) -> {
                                rootNode.setSelected(false);
                            });
                            rootNodeTextual.selectedProperty().addListener((ObservableValue<? extends Boolean> ov, Boolean old_val, Boolean new_val) -> {
                                rootNodeTextual.setSelected(false);
                            });

                            thisNode.put(0, thisItem);
                            thisNodeTextual.put(0, thisItemTextual);
                            nodosAtomList.add(thisNode);
                            nodosAtomList.get(nodosAtomList.size() - 1).values().iterator().next().setExpanded(true);
                            rootItemAtom = nodosAtomList.get(nodosAtomList.size() - 1).values().iterator().next();
                            nodosTextualList.add(thisNodeTextual);
                            nodosTextualList.get(nodosTextualList.size() - 1).values().iterator().next().setExpanded(true);
                            rootItemTextual = nodosTextualList.get(nodosTextualList.size() - 1).values().iterator().next();
                            
                            
//                            //BECAUSE AL FINAL DEL ATOMO
//                            rootItemTextual.expandedProperty().addListener((ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) -> {
//                                BooleanProperty bb = (BooleanProperty) observable;
//                                TreeItem t = (TreeItem) bb.getBean();
//                                System.out.println("BEAM "+ bb);
//                                if(bb.getValue())  {
//                                   rootItemTextualForListener.getValue().setText(previousText + " because");
//                                   if(rootItemTextualForListener.getChildren().size() > 0) {
//                                        String prevTextChild=  rootItemTextualForListener.getChildren().iterator().next().getValue().getText();
//                                        if(prevTextChild.startsWith("and ")) {
//                                          prevTextChild=  prevTextChild.substring(4);
//                                        }
//                                        rootItemTextualForListener.getChildren().iterator().next().getValue().setText(prevTextChild);
//                                   }
//                                   
//                                } else {
//                                    rootItemTextualForListener.getValue().setText(previousText);
//  
//                                }
//                            });
//                           rootItemTextual = rootItemTextualForListener;
                        //LOS DEMAS ATOMOS
                        } else {
                            if (level > maxLevel[s]) {
                                maxLevel[s] = level;
                            }
                            nodeIndex = findParentNodeList(nodosAtomList.size() - 1, level);

                         String previousText =  nodosTextualList.get(nodeIndex).values().iterator().next().getChildren().isEmpty() ?  "because " + arbolTextual.get(w).values().iterator().next() : "and " + arbolTextual.get(w).values().iterator().next();
                            
//                            int aux = getChilds(nodeIndex);
//                                 System.out.println(arbolAtoms.size() + " HIJOS "+ childs.length+ " INDES "+ nodeIndex + " SOY EL "+childs[0] + " " + childs[1] +" " +childs[2] );
                         
                            
                            CheckBox rootNode = new CheckBox(arbolAtoms.get(w).values().iterator().next() + "");
//                             System.out.println(""+childs.length + " "+childs[nodeIndex]);
                            CheckBox rootNodeTextual = new CheckBox(previousText );
//                            childs[nodeIndex]++;
//                            if (w == arbolAtoms.size() - 1) {
//                                rootNodeTextual = new CheckBox(arbolTextual.get(w).values().iterator().next());
//                            }
                            thisItem = new TreeItem<>(rootNode);
                            thisItemTextual = new TreeItem<>(rootNodeTextual);
                            thisItem.getValue().setId(s + "-" + w);
                            thisItemTextual.getValue().setId(s + "-" + w);
                            
                            
                            // BORRAR NODOS
                            final String thisItemID = thisItem.getValue().getId();
                            thisItem.getValue().setOnAction((event) -> {
                                Map<Integer, String> thisNodeToDelete = new HashMap<>();
                                thisNodeToDelete.put(tabPage, thisItemID);
                                nodesToDelete.add(thisNodeToDelete);
                            });

                            final String thisItemTextualID = thisItemTextual.getValue().getId();
                            thisItemTextual.getValue().setOnAction((event) -> {
                                Map<Integer, String> thisNodeToDeleteTextual = new HashMap<>();
                                thisNodeToDeleteTextual.put(tabPage, thisItemTextualID);
                                nodesToDelete.add(thisNodeToDeleteTextual);
                            });
                            
                            
                            //CONSRUIR ARBOL
                            thisNode.put(level, thisItem);
                            thisNodeTextual.put(level, thisItemTextual);
                            nodosAtomList.add(thisNode);
                            nodosTextualList.add(thisNodeTextual);
                            nodosAtomList.get(nodeIndex).values().iterator().next().getChildren().add(nodosAtomList.get(nodosAtomList.size() - 1).values().iterator().next());
                            TreeItem<CheckBox> nodoTextual = nodosTextualList.get(nodosTextualList.size() - 1).values().iterator().next();
//                            nodoTextual = deployingModifications(nodoTextual);
                            
                            nodosTextualList.get(nodeIndex).values().iterator().next().getChildren().add(nodoTextual);

                        treeRoot[s].setStyle("-fx-font-size:" + fontSize);
                        treeRoot[s].setRoot(showAtoms ? rootItemAtom : rootItemTextual);
                        selectionModel = treeRoot[s].getSelectionModel();
                        selectionModel.setSelectionMode(SelectionMode.MULTIPLE);
                    }
                    solTabs[s].setContent(treeRoot[s]);
                    }
                }
            }
        } catch (FileNotFoundException e) {
            System.out.println("[Error]: " + e.getMessage());
        }
    }

    /**
     * Changes the representation of the tree justification to atomic.
     *
     * @param event When clicking the radio button.
     */
    @FXML
    private void showAtom(ActionEvent event) {
        showAtoms = true;
        if (deployed) {
            nodesDeploy(event);
        } else if (contracted) {
            nodesContract(event);
        }
        applyZoom();
    }

    /**
     * Changes the representation of the tree justification to textual.
     *
     * @param event When clicking the radio button.
     */
    @FXML
    private void showPhrase(ActionEvent event) {
        showAtoms = false;
        if (deployed) {
            nodesDeploy(event);
        } else if (contracted) {
            nodesContract(event);
        }
        applyZoom();
    }

    /**
     * Gets the current justification in scene.
     *
     * @param event When clicking the a different justification.
     */
    @FXML
    private void getTabPage(MouseEvent event) {
        tabPage = tabPane.getSelectionModel().getSelectedIndex();
    }

    /**
     * Removes the selected nodes from the Tree Justification.
     *
     * @param event When clicking the 'Hide Elements' button.
     */
    @FXML
    private void hideElements(ActionEvent event) {

        try {
            int actualPage;
            for (int i = 0; i < nodesToDelete.size(); i++) {
                actualPage = nodesToDelete.get(i).keySet().iterator().next();
                if (tabPage != actualPage) {
                    alertIllegalHide();
                } else {
                    Map<Integer, TreeItem<CheckBox>> thisNodeDeleted = new HashMap<>();
                    for (int ia = 0; ia < nodosAtomList.size(); ia++) {
                        if (nodesToDelete.get(i).values().iterator().next().equals(nodosAtomList.get(ia).values().iterator().next().getValue().getId())) {
                            thisNodeDeleted.put(tabPage, new TreeItem(new CheckBox("      " + nodosAtomList.get(ia).values().iterator().next().getValue().getText() + "   =>   " + nodosTextualList.get(ia).values().iterator().next().getValue().getText())));
                            deletedNodes.add(thisNodeDeleted);
                        }

                    }
                    deleteNodes(nodesToDelete.get(i).values().iterator().next());
                }
            }
            nodesToDelete.clear();
        } catch (InputMismatchException e) {
            System.out.println("[Error]: " + e.getMessage() + " at hideElements");
        }
    }

    /**
     * Collapses the Justification Tree.
     *
     * @param event When clicking the 'Contract' radio button.
     */
    @FXML
    private void nodesContract(ActionEvent event) {
        nodesContractAux();
    }

    /**
     * Collapses the Justification Tree.
     */
    private void nodesContractAux() {
        for (int i = 0; i < currentLevel.length; i++) {
            currentLevel[i] = 0;
        }
        deployed = false;
        contracted = true;
        for (int i = 0; i < nodosAtomList.size(); i++) {
            nodosAtomList.get(i).values().iterator().next().setExpanded(false);
            nodosTextualList.get(i).values().iterator().next().setExpanded(false);
        }
    }

    /**
     * Expands the Justification Tree.
     *
     * @param event When clicking the 'Deploy' radio button.
     */
    @FXML
    private void nodesDeploy(ActionEvent event) {
        nodesDeployAux();
    }

    /**
     * Expands the Justification Tree.
     */
    private void nodesDeployAux() {
        System.arraycopy(maxLevel, 0, currentLevel, 0, currentLevel.length);
        deployed = true;
        contracted = false;
        for (int i = 0; i < nodosAtomList.size(); i++) {
            nodosAtomList.get(i).values().iterator().next().setExpanded(true);
            nodosTextualList.get(i).values().iterator().next().setExpanded(true);
        }
    }

    /**
     * Applies the specified zoom level.
     */
    private void applyZoom() {
        FXMLLoader loader = new FXMLLoader(getClass().getResource("HiddenNodes.fxml"));
        try {
            Parent rootScene = loader.load();
        } catch (IOException e) {
            System.out.println("[Error] " + e.getMessage() + " at applyZoom");
        }

        HiddenNodesController hiddenNodesController = loader.<HiddenNodesController>getController();
        hiddenNodesController.initData(deletedNodes);

        atomsToHide = null;
        tabPane.getTabs().removeAll(tabPane.getTabs());
        fontSize = scrollZoom.getValue() < 18 ? 12 : scrollZoom.getValue() < 30 ? 19 : 30;
        rebuild(fontSize);
        if (deployed) {
            nodesDeployAux();
        }
        if (contracted) {
            nodesContractAux();
        }
    }

    /**
     * Expands 1 level of the Justification Tree.
     * 
     * @param event When clicking the '+ 1 Level' button.
     */
    @FXML
    private void add1level(ActionEvent event) {
        for (int i = 0; i < nodosAtomList.size(); i++) {
            if (nodosAtomList.get(i).keySet().iterator().next() == currentLevel[tabPage]) {
                nodosAtomList.get(i).values().iterator().next().setExpanded(true);
            }
            if (nodosTextualList.get(i).keySet().iterator().next() == currentLevel[tabPage]) {
                nodosTextualList.get(i).values().iterator().next().setExpanded(true);
            }
        }
        for (int i = 0; i < currentLevel.length; i++) {
            if (currentLevel[i] < maxLevel[i]) {
                currentLevel[i]++;
            }
        }
    }

    /**
     * Collapses 1 level of the Justification Tree.
     *
     * @param event When clicking the '- 1 Level' button.
     */
    @FXML
    private void minus1Level(ActionEvent event) {
        for (int i = currentLevel.length - 1; i >= 0; i--) {
            if (currentLevel[i] > minLevel) {
                currentLevel[i]--;
            }
        }
        for (int i = 0; i < nodosAtomList.size(); i++) {
            if (nodosAtomList.get(i).keySet().iterator().next() == currentLevel[tabPage]) {
                nodosAtomList.get(i).values().iterator().next().setExpanded(false);
            }
            if (nodosTextualList.get(i).keySet().iterator().next() == currentLevel[tabPage]) {
                nodosTextualList.get(i).values().iterator().next().setExpanded(false);
            }
        }
    }

    /**
     * Deletes a node, refloating its children when necessary.
     *
     * @param id The node ID to be removed.
     */
    private void deleteNodes(String id) {
        List<Map<Integer,TreeItem<CheckBox>>> newNodosAtomList = nodosAtomList,  newNodosTextualList = new ArrayList<>();
        
        TreeItem<CheckBox> toDelete, toDeleteSibling, toDeleteTextual, thisNode, thisNewNode;
        int level;
        for (int i = 0; i < nodosAtomList.size(); i++) {
            level = nodosAtomList.get(i).keySet().iterator().next();
            toDelete = nodosAtomList.get(i).values().iterator().next();
            toDeleteTextual = nodosTextualList.get(i).values().iterator().next();
            ObservableList<TreeItem<CheckBox>> toDeleteChildren = toDelete.getChildren();
            ObservableList<TreeItem<CheckBox>> toDeleteChildrenTextual = toDeleteTextual.getChildren();

            if ((level > 0) && id.equals(nodosAtomList.get(i).values().iterator().next().getValue().getId())) {

                  if (toDeleteChildren != null) {
                   
                        toDelete.getParent().getChildren().addAll(toDeleteChildren);
                        toDeleteTextual.getParent().getChildren().addAll(toDeleteChildrenTextual);
                        toDeleteTextual.getParent().getChildren().remove(toDeleteTextual);
                  }
                  toDelete.getParent().getChildren().remove(toDelete);

                               nodesInTab[tabPage]--;
            
            }else if(level > 0){

//                 toDelete = nodosAtomList.get(i).values().iterator().next();
                 System.err.println(toDelete.getParent());
//  // //                 if(!toDeleteChildren.isEmpty()) toDelete.getParent().getChildren().addAll(toDeleteChildren);
                   toDeleteSibling = toDelete;
//                   toDelete.getParent().getChildren().add(toDeleteSibling);
//                   toDelete.getParent().getChildren().remove(toDelete);
                    

                
            }
               
        }
//            nodosAtomList = newNodosAtomList;
//                System.out.println("LOLOLOL " + nodosAtomList.size() + " " + newNodosAtomList.size());
//            nodosTextualList = newNodosTextualList;
            tabPane.getTabs().removeAll(tabPane.getTabs());
            rebuild(fontSize);
 
        
        
        
//        TreeItem<CheckBox> toDelete = null, toDeleteSibling = null, toDeleteTextual = null;
//        int indise = 0, level = 0;
//        ObservableList<TreeItem<CheckBox>> toDeleteChildren = null, toDeleteChildrenTextual = null;
//        for (int i = 0; i < nodosAtomList.size(); i++) {
//             level = nodosAtomList.get(i).keySet().iterator().next();
//            if (id.equals(nodosAtomList.get(i).values().iterator().next().getValue().getId())) {
//                indise = i;
//            }
//        }
//        if (indise != 0) {
//            toDelete = nodosAtomList.get(indise).values().iterator().next();
//            toDeleteTextual = nodosTextualList.get(indise).values().iterator().next();
//            toDeleteChildren = toDelete.getChildren();
//            toDeleteChildrenTextual = toDeleteTextual.getChildren();
//            if (toDeleteChildren != null || toDeleteChildrenTextual != null) {
//                if (toDeleteChildren != null) {
//                    for (int k = 1; k <= toDeleteChildren.size(); k++) {
//                        Map<Integer, TreeItem<CheckBox>> filho = nodosAtomList.get(k + indise);
//                        int prevLe = filho.keySet().iterator().next();
//                        TreeItem<CheckBox> val = filho.values().iterator().next();
//                        filho.clear();
//                        filho.put(--prevLe, val);
//                    }
//                    toDelete.getParent().getChildren().addAll(toDeleteChildren);
//                    toDelete.getParent().getChildren().remove(toDelete);
//                    toDeleteTextual.getParent().getChildren().addAll(toDeleteChildrenTextual);
//                    toDeleteTextual.getParent().getChildren().remove(toDeleteTextual);
//                      System.err.println("AOUT "+nodosAtomList.size());
//                } else {
//                    for (int k = 1; k <= toDeleteChildrenTextual.size(); k++) {
//                        Map<Integer, TreeItem<CheckBox>> filho = nodosTextualList.get(k + indise);
//                        int prevLe = filho.keySet().iterator().next();
//                        TreeItem<CheckBox> val = filho.values().iterator().next();
//                        filho.clear();
//                        filho.put(--prevLe, val);
//                    }
//                    toDelete.getParent().getChildren().addAll(toDeleteChildren);
//                    toDelete.getParent().getChildren().remove(toDelete);
//                    toDeleteTextual.getParent().getChildren().addAll(toDeleteChildrenTextual);
//                    toDeleteTextual.getParent().getChildren().remove(toDeleteTextual);
//                }
//
//            }
//            nodosAtomList.remove(indise);
//            nodosTextualList.remove(indise);
//            nodesInTab[tabPage]--;
//
//        } else {
////                   if(level > 0){
////                   if(!toDeleteChildren.isEmpty()) toDelete.getParent().getChildren().addAll(toDeleteChildren);
////                   toDeleteSibling = toDelete;
////                  toDelete.getParent().getChildren().add(toDeleteSibling);
////                    toDelete.getParent().getChildren().remove(toDelete);
////
////                
////            }
//        }
////            nodosAtomList = newNodosAtomList;
//
//            tabPane.getTabs().removeAll(tabPane.getTabs());
//            rebuild(fontSize);
    }

    /**
     * Rebuilds the Justification Tree with the specified settings, such as zoom
     * level, removed nodes, representation...
     *
     * @param fontSize The font size (or zoom level).
     */
    private void rebuild(int fontSize) {
      //  System.err.println("ATOMOSSS "+ nodosAtomList.size());
        try {
            Map<Integer, TreeItem<CheckBox>> thisNode, thisNodeTextual;
            TreeItem<CheckBox> thisItem, thisItemTextual;
            maxLevel = new int[solutions.length];
            currentLevel = new int[solutions.length];

            for (int s = 0; s < solutions.length; s++) {
                currentLevel[s] = 1;
                treeRoot[s] = new TreeView<>();

                solTabs[s] = new Tab("Justification " + (s + 1));
                solTabs[s].setClosable(false);
                tabPane.getTabs().add(solTabs[s]);

                tabPane.setSide(Side.BOTTOM);
                tabPane.getSelectionModel().select(tabPage);

                TreeItem<CheckBox> rootItemAtom = null;
                TreeItem<CheckBox> rootItemTextual = null;
                int nodeIndex;
                try {
                    aggregateNodes += nodesInTab[s - 1];

                } catch (Exception e) {
                    aggregateNodes = 0;
                }
                for (int w = 0; w < nodesInTab[s]; w++) {
//                    System.err.println(nodosAtomList.get(w + aggregateNodes));
                    int level = (int) nodosAtomList.get(w + aggregateNodes).keySet().iterator().next();
                    if (level == 0) {
//                        System.err.println(w+"-"+nodosAtomList.get(w + +aggregateNodes).values().iterator().next().getValue().getText());
                        CheckBox rootNode = new CheckBox(nodosAtomList.get(w + +aggregateNodes).values().iterator().next().getValue().getText());
                        CheckBox rootNodeTextual = new CheckBox(nodosTextualList.get(w + +aggregateNodes).values().iterator().next().getValue().getText() + " because");
                        thisNode = new HashMap<>();
                        thisNodeTextual = new HashMap<>();
                        thisItem = new TreeItem<>(rootNode);
                        thisItemTextual = new TreeItem<>(rootNodeTextual);
                        thisItem.getValue().setId(s + "-" + w);
                        thisItemTextual.getValue().setId(s + "-" + w);

                        // IMPIDE LA SELECCION DEL ROOT NODE
                        rootNode.selectedProperty().addListener((ObservableValue<? extends Boolean> ov, Boolean old_val, Boolean new_val) -> {
                            rootNode.setSelected(false);
                        });

                        thisNode.put(0, thisItem);
                        thisNodeTextual.put(0, thisItemTextual);

                        nodosAtomList.get(nodosAtomList.size() - 1).values().iterator().next().setExpanded(true);
                        nodosTextualList.get(nodosTextualList.size() - 1).values().iterator().next().setExpanded(true);
                        rootItemAtom = nodosAtomList.get(w + aggregateNodes).values().iterator().next();
//                        System.out.println("ROOT "+rootItemAtom);
                        rootItemTextual = nodosTextualList.get(w + aggregateNodes).values().iterator().next();
                    } else {
//                      System.err.println(w+"-"+nodosAtomList.get(w + +aggregateNodes).values().iterator().next().getValue().getText());
                        if (level > maxLevel[s]) {
                            maxLevel[s] = level;
                        }

                        nodeIndex = findParentNodeList(nodosAtomList.size() - 1, level);
                        CheckBox rootNode = new CheckBox(nodosAtomList.get(w + +aggregateNodes).values().iterator().next().getValue().getText());
                        CheckBox rootNodeTextual = new CheckBox(nodosTextualList.get(w + +aggregateNodes).values().iterator().next().getValue().getText() + " and");
                        if (w == arbolAtoms.size() - 1) {
                            rootNodeTextual = new CheckBox(arbolTextual.get(w).values().iterator().next());
                        }
                        thisNode = new HashMap<>();
                        thisNodeTextual = new HashMap<>();
                        thisItem = new TreeItem<>(rootNode);
                        thisItemTextual = new TreeItem<>(rootNodeTextual);
                        thisItem.getValue().setId(s + "-" + w);
                        thisItemTextual.getValue().setId(s + "-" + w);
                        final String thisItemID = thisItem.getValue().getId();
                        thisItem.getValue().setOnAction((event) -> {
                            Map<Integer, String> thisNodeToDelete = new HashMap<>();
                            thisNodeToDelete.put(tabPage, thisItemID);
                            nodesToDelete.add(thisNodeToDelete);
                        });
                        thisNode.put(level, thisItem);
                        final String thisItemTextualID = thisItemTextual.getValue().getId();
                        thisItemTextual.getValue().setOnAction((event) -> {
                            Map<Integer, String> thisNodeToDeleteTextual = new HashMap<>();
                            thisNodeToDeleteTextual.put(tabPage, thisItemTextualID);
                            nodesToDelete.add(thisNodeToDeleteTextual);
                            System.err.println(nodesToDelete.size());
                        });
                        thisNodeTextual.put(level, thisItemTextual);
                    }
                    treeRoot[s].setStyle("-fx-font-size:" + fontSize);
                    treeRoot[s].setRoot(null);
                    treeRoot[s].setRoot(showAtoms ? rootItemAtom : rootItemTextual);
                    selectionModel = treeRoot[s].getSelectionModel();
                    selectionModel.setSelectionMode(SelectionMode.MULTIPLE);
                    
                    
//                    for(int x = 0; x < rootItemAtom.getChildren().size(); x++) {
                   // System.err.println("-"+rootItemAtom);
//                    }
                }
                
                solTabs[s].setContent(treeRoot[s]);
            }
        } catch (InputMismatchException e) {
            System.out.println("[Error]: " + e.getMessage());
        }
    }

    /**
     * Opens the "Hidden Nodes" window, which contains a representation of the
     * nodes erased by the user.
     *
     * @param event When clicking the leading-right icon above the table.
     * @throws IOException If it can not open the "Hidden Nodes" window.
     */
    @FXML
    private void openHiddenNodes(MouseEvent event) throws IOException {
        FXMLLoader loader = new FXMLLoader(getClass().getResource("HiddenNodes.fxml"));
        Parent rootScene = loader.load();

        HiddenNodesController hiddenNodesController = loader.<HiddenNodesController>getController();
        hiddenNodesController.initData(deletedNodes);

        Stage stageChild = new Stage();
        stageChild.setTitle("HIDDEN NODES");
        stageChild.getIcons().add(new Image(Main.class.getResourceAsStream("/images/Back.png")));
        Scene scene = new Scene(rootScene);

        stageChild.initOwner(stage);
        stageChild.setScene(scene);
        stageChild.setResizable(false);
        stageChild.initModality(Modality.APPLICATION_MODAL);
        stageChild.showAndWait();

    }

    /**
     * Gets the values of 'ctrl', '+' and '-' to Zoom In or Zoom Out.
     *
     * @param control Either 'ctrl' key is pressed or not.
     * @param plus Either '+' key is pressed or not.
     * @param minus Either '-' key is pressed or not.
     */
    public void initData(boolean control, boolean plus, boolean minus) {
        this.ctrlPressed = control;
        this.plusPressed = plus;
        this.minusPressed = minus;
        int zoomValue = (int) scrollZoom.getValue();

        if (this.ctrlPressed && this.plusPressed) {
            if (zoomValue < 18) {
                scrollZoom.setValue(24.0);
            } else if (zoomValue < 30) {
                scrollZoom.setValue(36.0);
            }
            applyZoom();
        }
        if (this.ctrlPressed && this.minusPressed) {
            if (zoomValue > 30) {
                scrollZoom.setValue(24.0);
            } else if (zoomValue > 18) {
                scrollZoom.setValue(0.0);
            }
            applyZoom();
        }
    }
    
        /**
     * Changes the displayed text when deploying nodes.
     *
     * @param nodoTextual The node that will be retexted.
     * @return The node retexted.
     * @deprecated
     */
    private TreeItem<CheckBox> deployingModifications(TreeItem<CheckBox> nodoTextual) {

      String previousTextElse = nodoTextual.getValue().getText();
                                nodoTextual.expandedProperty().addListener((ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) -> {
                                BooleanProperty bb = (BooleanProperty) observable;
                                TreeItem t = (TreeItem) bb.getBean();
                                if(bb.getValue() && nodoTextual.getChildren().size() > 0 )  {                              
                                   nodoTextual.getValue().setText(previousTextElse + " because");                          
                                } else {
                                    nodoTextual.getValue().setText(previousTextElse);

                                }
                            });
                           return nodoTextual;
    }
}
