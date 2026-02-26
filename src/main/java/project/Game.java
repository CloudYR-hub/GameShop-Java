package project;

/**
 * Game class for a game store.
 * Represents a game with a name, price, category (genre), and quantity.
 * @author andand5
 */
class Game {
    private final String name;
    private final double price;
    private final String category; // Genre of the game
    private int quantity;

    /**
     * Constructor for the Game class.
     * @param name Name of the game.
     * @param price Price of the game.
     * @param category Genre of the game.
     */
    public Game(String name, double price, String category) {
        this.name = name;
        this.price = price;
        this.category = category;
        this.quantity = 1;
    }

    /**
     * Gets the name of the game.
     * @return Name of the game.
     */
    public String getName() {
        return name;
    }

    /**
     * Gets the price of the game.
     * @return Price of the game.
     */
    public double getPrice() {
        return price;
    }

    /**
     * Gets the genre of the game.
     * @return Genre of the game.
     */
    public String getCategory() {
        return category;
    }

    /**
     * Gets the quantity of this game in stock.
     * @return Quantity in stock.
     */
    public int getQuantity() {
        return quantity;
    }

    /**
     * Sets the quantity of this game in stock.
     * @param quantity Quantity to set.
     */
    public void setQuantity(int quantity) {
        this.quantity = quantity;
    }
}
