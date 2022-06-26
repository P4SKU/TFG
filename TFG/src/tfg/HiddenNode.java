package tfg;

/**
 * This class creates a Hidden Node from the original Justification Tree.
 *
 * @author Pasku
 */
public class HiddenNode {

    /**
     * The node text.
     */
    private String node = null;

    /**
     * The justification where the node belongs to.
     */
    private String page = null;

    /**
     * A Hidden Node consists the node itself and the justification (page) where
     * it appears.
     *
     * @param page The justification where the node appears.
     * @param node The node text.
     */
    // When seeing the hidden nodes, we do not want to see the "and" added to the atoms on textual representation.
    public HiddenNode(String page, String node) {

        this.node = node.replace(" and", "");
        this.page = page.replace(" and", "");
    }

    /**
     * @deprecated
     */
    public HiddenNode() {
    }

    /**
     * @return The text of the node.
     */
    public String getNode() {
        return node;
    }

    /**
     * @param node The text of the node.
     */
    public void setNode(String node) {
        this.node = node;
    }

    /**
     * @return The page (justification) where the node appears.
     */
    public String getPage() {
        return page;
    }

    /**
     * @param page The page (justification) where the node appears.
     */
    public void setPage(String page) {
        this.page = page;
    }

    /**
     * @return A string containing the text of the node and the justification
     * where it appears.
     */
    @Override
    public String toString() {
        return "[NODE]: " + this.node + " || [PAGE]: " + this.page;
    }

}
