package project;

import javax.swing.*;
import javax.swing.border.Border;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.util.List;
import java.util.Map;

/**
 * GUI class that showcases a game store shopping cart with advanced design.
 * @author andand5
 */
public class GameShopGUI extends JFrame implements ActionListener {
    private final JTextArea taDisplay = new JTextArea();
    private final JButton btnLoad = new JButton("Load Games from File");
    private final JButton btnAdd = new JButton("Add Game");
    private final JButton btnDisplay = new JButton("Display Games");
    private final JButton btnGroup = new JButton("Group By Genre");
    private final JButton btnFilter = new JButton("Filter By Price");
    private final JButton btnRemove = new JButton("Remove By Genre");
    private final JButton btnSearch = new JButton("Search Game");
    private final JButton btnUnset = new JButton("Unset Game");
    private final DynamicArrayClass<Game> dynamicArray = new DynamicArrayClass<>();
    private final JFileChooser fileChooser = new JFileChooser();

    public GameShopGUI() {
        initComponents();
    }

    private void initComponents() {
        setTitle("Game Store");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLayout(new BorderLayout());
        setSize(800, 600);
        setLocationRelativeTo(null);

        // Text display panel
        taDisplay.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 14));
        taDisplay.setEditable(false);
        taDisplay.setBackground(Color.WHITE);
        taDisplay.setForeground(Color.DARK_GRAY);
        taDisplay.setBorder(BorderFactory.createLineBorder(Color.GRAY, 2));
        JScrollPane scrollDisplay = new JScrollPane(taDisplay);
        scrollDisplay.setBorder(BorderFactory.createTitledBorder("Game Information"));

        // Tabbed pane for controls
        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 14));

        // Control panels
        tabbedPane.add("Manage Games", createManageGamesPanel());
        tabbedPane.add("Game Analysis", createAnalysisPanel());

        // Adding components
        add(scrollDisplay, BorderLayout.CENTER);
        add(tabbedPane, BorderLayout.EAST);

        setVisible(true);
    }

    private JPanel createManageGamesPanel() {
        JPanel managePanel = new JPanel();
        managePanel.setLayout(new GridLayout(4, 2, 10, 10));
        managePanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        // Buttons for management
        customizeButton(btnLoad, managePanel);
        customizeButton(btnAdd, managePanel);
        customizeButton(btnDisplay, managePanel);
        customizeButton(btnSearch, managePanel);
        customizeButton(btnUnset, managePanel);
        customizeButton(btnRemove, managePanel);

        return managePanel;
    }

    private JPanel createAnalysisPanel() {
        JPanel analysisPanel = new JPanel();
        analysisPanel.setLayout(new GridLayout(2, 1, 10, 10));
        analysisPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        // Buttons for analysis
        customizeButton(btnGroup, analysisPanel);
        customizeButton(btnFilter, analysisPanel);

        return analysisPanel;
    }

    private void customizeButton(JButton button, JPanel panel) {
        button.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 14));
        button.setBackground(new Color(70, 130, 180));
        button.setForeground(Color.WHITE);
        button.setFocusPainted(false);
        button.addActionListener(this);
        panel.add(button);
    }

    @Override
    public void actionPerformed(ActionEvent ae) {
        Object source = ae.getSource();

        if (source.equals(btnLoad)) {
            loadFromFile();
        } else if (source.equals(btnAdd)) {
            addGame();
        } else if (source.equals(btnDisplay)) {
            displayGames();
        } else if (source.equals(btnGroup)) {
            groupByGenre();
        } else if (source.equals(btnFilter)) {
            filterByPrice();
        } else if (source.equals(btnRemove)) {
            removeByGenre();
        } else if (source.equals(btnSearch)) {
            searchGame();
        } else if (source.equals(btnUnset)) {
            unsetGame();
        }
    }

    private void loadFromFile() {
        int returnValue = fileChooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            File file = fileChooser.getSelectedFile();
            try (BufferedReader br = new BufferedReader(new FileReader(file))) {
                String line;
                while ((line = br.readLine()) != null) {
                    String[] parts = line.split(",");
                    if (parts.length == 4) {
                        String title = parts[0].trim();
                        double price = Double.parseDouble(parts[1].trim());
                        String genre = parts[2].trim();
                        int quantity = Integer.parseInt(parts[3].trim());
                        dynamicArray.add(new Game(title, price, genre));
                    }
                }
                taDisplay.setText("Games loaded successfully from: " + file.getName());
            } catch (IOException | NumberFormatException e) {
                taDisplay.setText("Error: " + e.getMessage());
            }
        }
    }

    private void addGame() {
        String title = JOptionPane.showInputDialog(this, "Enter game title:");
        if (title == null || title.trim().isEmpty()) {
            taDisplay.setText("Please enter a valid game title.");
            return;
        }

        String priceInput = JOptionPane.showInputDialog(this, "Enter game price:");
        double price;
        try {
            price = Double.parseDouble(priceInput);
            if (price < 0) {
                taDisplay.setText("Price cannot be negative");
                return;
            }
        } catch (NumberFormatException e) {
            taDisplay.setText("Invalid price. Please enter a numeric value");
            return;
        }

        String genre = JOptionPane.showInputDialog(this, "Enter game genre:");
        if (genre == null || genre.trim().isEmpty()) {
            taDisplay.setText("Please enter a valid game genre");
            return;
        }

        Game game = new Game(title.trim(), price, genre.trim());
        dynamicArray.add(game);
        taDisplay.setText("Game added:\n" +
                "Title: " + game.getName() + "\n" +
                "Price: $" + game.getPrice() + "\n" +
                "Genre: " + game.getCategory());
    }

    private void displayGames() {
        List<Game> games = dynamicArray.getAllItems();
        StringBuilder displayText = new StringBuilder("Games in Cart:\n");
        for (Game game : games) {
            if (game != null) {
                displayText.append(game.getName())
                        .append(" - ").append(game.getCategory())
                        .append(" - $").append(game.getPrice()).append("\n");
            } else {
                displayText.append("[Unset Game]\n");
            }
        }
        taDisplay.setText(displayText.toString());
    }

    private void groupByGenre() {
        Map<String, List<Game>> groupedGames = dynamicArray.groupByGenre();
        StringBuilder displayText = new StringBuilder("Games Grouped By Genre:\n");
        for (String genre : groupedGames.keySet()) {
            displayText.append("Genre: ").append(genre).append("\n");
            for (Game game : groupedGames.get(genre)) {
                displayText.append("  - ").append(game.getName())
                        .append(" - $").append(game.getPrice()).append("\n");
            }
            displayText.append("\n");
        }
        taDisplay.setText(displayText.toString());
    }

    private void filterByPrice() {
        String minPriceInput = JOptionPane.showInputDialog(this, "Enter minimum price:");
        String maxPriceInput = JOptionPane.showInputDialog(this, "Enter maximum price:");

        if (minPriceInput != null && !minPriceInput.isEmpty() && maxPriceInput != null && !maxPriceInput.isEmpty()) {
            try {
                double minPrice = Double.parseDouble(minPriceInput);
                double maxPrice = Double.parseDouble(maxPriceInput);

                if (minPrice <= maxPrice) {
                    List<Game> filteredGames = dynamicArray.filterByPrice(minPrice, maxPrice);
                    StringBuilder displayText = new StringBuilder("Games Filtered By Price Range:\n");
                    for (Game game : filteredGames) {
                        displayText.append(game.getName())
                                .append(" - $").append(game.getPrice())
                                .append(" - ").append(game.getCategory()).append("\n");
                    }
                    taDisplay.setText(displayText.toString());
                } else {
                    taDisplay.setText("Invalid range: Minimum price cannot exceed maximum price.");
                }
            } catch (NumberFormatException e) {
                taDisplay.setText("Invalid input: Please enter valid numeric prices.");
            }
        } else {
            taDisplay.setText("Both minimum and maximum prices are required.");
        }
    }

    private void removeByGenre() {
        JPanel panel = new JPanel(new GridLayout(1, 2, 10, 10));
        JLabel label = new JLabel("Genre to Remove:");
        JTextField textField = new JTextField();
        panel.add(label);
        panel.add(textField);

        int result = JOptionPane.showConfirmDialog(this, panel, "Remove by Genre", JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE);
        if (result == JOptionPane.OK_OPTION) {
            String genreToRemove = textField.getText();
            if (genreToRemove != null && !genreToRemove.trim().isEmpty()) {
                dynamicArray.removeGamesByGenre(genreToRemove.trim());
                taDisplay.setText("Games removed from genre: " + genreToRemove);
                displayGames();
            } else {
                taDisplay.setText("Genre cannot be empty.");
            }
        }
    }


    private void unsetGame() {
        String indexInput = JOptionPane.showInputDialog(this, "Enter index to unset:");
        try {
            int index = Integer.parseInt(indexInput);
            dynamicArray.unset(index);
            taDisplay.setText("Game at index " + index + " has been unset.");
            displayGames();
        } catch (NumberFormatException e) {
            taDisplay.setText("Invalid input: Please enter a numeric index.");
        } catch (IndexOutOfBoundsException e) {
            taDisplay.setText("Error: " + e.getMessage());
        }
    }

    private void searchGame() {
        JPanel panel = new JPanel(new GridLayout(1, 2, 10, 10));
        JLabel label = new JLabel("Game Title:");
        JTextField textField = new JTextField();
        panel.add(label);
        panel.add(textField);

        int result = JOptionPane.showConfirmDialog(this, panel, "Search Game", JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE);
        if (result == JOptionPane.OK_OPTION) {
            String searchTitle = textField.getText();
            if (searchTitle != null && !searchTitle.trim().isEmpty()) {
                Game foundGame = dynamicArray.getAllItems().stream()
                        .filter(game -> game != null && game.getName().equalsIgnoreCase(searchTitle.trim()))
                        .findFirst()
                        .orElse(null);

                if (foundGame != null) {
                    taDisplay.setText("Game Found:\n" +
                            "Title: " + foundGame.getName() + "\n" +
                            "Price: $" + foundGame.getPrice() + "\n" +
                            "Genre: " + foundGame.getCategory());
                } else {
                    taDisplay.setText("No game found with the title: " + searchTitle);
                }
            } else {
                taDisplay.setText("Search title cannot be empty.");
            }
        }
    }



    public static void main(String[] args) {
        SwingUtilities.invokeLater(GameShopGUI::new);
    }
}
