package tfg;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeTableColumn;
import javafx.scene.control.TreeTableView;
import javafx.scene.control.cell.TreeItemPropertyValueFactory;

/**
 * This class contains the nodes that have been hidden by the user.
 * <br><br>
 *
 * @author Pasku
 */
public class HiddenNodesController implements Initializable {

    /**
     * The main TreeTableView container.
     */
    @FXML
    TreeTableView<String> treeTableView = new TreeTableView<>();
    /**
     * Column of the TreeTableView (Justification number).
     */
    @FXML
    private TreeTableColumn<String, String> justificationColumn;
    /**
     * Column of the TreeTableView (Node, atomic and textual representation).
     */
    @FXML
    private TreeTableColumn<String, String> nodeColumn;
    /**
     * List of the deleted nodes.
     */
    private List<Map<Integer, TreeItem<CheckBox>>> nodesDeleted = new ArrayList<>();
    /**
     * Justifications (pages) already added.
     */
    private List<String> ownPages = new ArrayList<>();
    /**
     * Justification (page) where the node comes from.
     */
    private List<TreeItem> pages = new ArrayList<>();
    /**
     * Justification (page) where the node belongs to.
     */
    private int index = 0;
    /**
     * Global tree item - Comes in handy for appending childs.
     */
    TreeItem justifications = new TreeItem(new HiddenNode("", "Click each Justification to Deploy / Contract nodes"));

    /**
     * Sets the global tree item expandaded and awaits for new hidden nodes to
     * arrive.
     *
     * @param url URL
     * @param rb ResourceBundle
     */
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        // When there are no hidden nodes
        treeTableView.setPlaceholder(new Label("There are no hidden nodes"));
        // Setting the global tree item expandaded forever
        justifications.setExpanded(true);
        justifications.expandedProperty().addListener((ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) -> {
            justifications.setExpanded(true);
        });

        // Setting the retriever methods for both column attributes.
        justificationColumn.setCellValueFactory(new TreeItemPropertyValueFactory<>("page"));
        nodeColumn.setCellValueFactory(new TreeItemPropertyValueFactory<>("node"));

    }

    /**
     * Gets the hidden nodes to append to the TreeTableView.
     *
     * @param receivedDeletedNodes The list of hidden nodes to append.
     */
    public void initData(List<Map<Integer, TreeItem<CheckBox>>> receivedDeletedNodes) {
        nodesDeleted = receivedDeletedNodes;
        treeTableView.setShowRoot(false);

        for (int i = 0; i < receivedDeletedNodes.size(); i++) {
            buildtreeTableView(receivedDeletedNodes.get(i).keySet().iterator().next().toString(), receivedDeletedNodes.get(i).values().iterator().next().getValue().getText());
        }
    }

    /**
     * Appends the hidden nodes to the TreeTableView.
     *
     * @param fromPage The page where it belongs.
     * @param node The node text.
     */
    private void buildtreeTableView(String fromPage, String node) {
        /**
         * Checks if the justification was previously added.
         */
        boolean repeatedPage = false;
        /**
         * Shows if a new justification was added .
         */
        boolean added = false;

        for (int i = 0; i < ownPages.size() && !repeatedPage; i++) {
            if (ownPages.get(i).equals(fromPage)) {
                repeatedPage = true;
                index = Integer.parseInt(fromPage);
            }
        }
        if (!repeatedPage) {
            ownPages.add(fromPage);
            TreeItem item = new TreeItem(new HiddenNode(("Justification " + (Integer.parseInt(fromPage) + 1)), ""));
            item.setExpanded(true);
            pages.add(item);
            index = Integer.parseInt(fromPage);
        }

        TreeItem nodeDeleted = new TreeItem(new HiddenNode("", node));

        for (int i = 0; i < ownPages.size() && !added; i++) {
            if (index == Integer.parseInt(ownPages.get(i))) {
                pages.get(i).getChildren().add(nodeDeleted);
                if (!repeatedPage) {
                    justifications.getChildren().add(pages.get(i));
                }
                added = true;
            }
        }
        treeTableView.setRoot(justifications);
    }

}
