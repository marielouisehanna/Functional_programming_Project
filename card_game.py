import json
import os
import pygame

class CardGame:
    def __init__(self, game_data):
        self.game_name = game_data['gameName']
        self.num_players = game_data['numPlayers']
        self.cards = game_data['cards']
        self.rules = game_data['rules']
        self.actions = game_data['actions']
        self.screen = None
        self.card_images = {}
        self.font = None

    def load_card_images(self):
        """Load card images from the 'cards/' directory."""
        for card in self.cards:
            card_name = card['name']
            image_path = f"cards/{card_name}.png"
            if os.path.exists(image_path):
                self.card_images[card_name] = pygame.image.load(image_path)
            else:
                print(f"Warning: Image for card '{card_name}' not found.")

    def setup_pygame(self):
        """Initialize pygame and set up the display."""
        pygame.init()
        self.screen = pygame.display.set_mode((800, 600))
        pygame.display.set_caption(self.game_name)
        self.font = pygame.font.Font(None, 36)

    def display_text(self, text, position):
        """Display text on the screen."""
        text_surface = self.font.render(text, True, (255, 255, 255))
        self.screen.blit(text_surface, position)

    def display_cards(self):
        """Display all cards on the screen."""
        x, y = 50, 100  # Starting position for cards
        for card in self.cards:
            card_name = card['name']
            if card_name in self.card_images:
                self.screen.blit(self.card_images[card_name], (x, y))
                self.display_text(card_name, (x + 30, y + 310))  # Display card name below the card
                x += 220  # Move to the next position
                if x > 600:  # Wrap to the next row
                    x = 50
                    y += 350

    def main_loop(self):
        """Main game loop."""
        running = True
        while running:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False

            self.screen.fill((0, 0, 0))  # Clear the screen
            self.display_text(f"Game: {self.game_name}", (20, 20))
            self.display_cards()
            pygame.display.flip()

        pygame.quit()

    def start_game(self):
        """Start the game."""
        self.setup_pygame()
        self.load_card_images()
        self.main_loop()

if __name__ == "__main__":
    # Load the game data from the generated game.json
    with open('game.json', 'r') as f:
        game_data = json.load(f)
    game = CardGame(game_data)
    game.start_game()
