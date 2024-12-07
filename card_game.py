import pygame
import json
import random


def min_indexes(cards):
    values = [card["value"] for card in cards]
    min_val = min(values)
    min_ind = []
    for i in range(len(values)):
        if values[i] == min_val:
            min_ind.append(i)
    return min_ind

def max_indexes(cards):
    values = [card["value"] for card in cards]
    max_val = max(values)
    max_ind = []
    for i in range(len(values)):
        if values[i] == max_val:
            max_ind.append(i)
    return max_ind


# Initialize Pygame
pygame.init()
screen = pygame.display.set_mode((1200, 800))
pygame.display.set_caption("Interactive Card Game")

# Load game data
with open("game.json", "r") as f:
    game_data = json.load(f)

# Extract game data
game_name = game_data["gameName"]
player_count = game_data["players"]
player_count+=1
rounds = game_data["rounds"]
cards = game_data["cards"]
rules = game_data["rules"]
options = game_data["options"]

# Determine cards per player
default_cards_per_player = 4
cards_per_player = next(
    (int(opt["value"]) for opt in options if opt["name"] == "cards-per-player"),
    default_cards_per_player
)

empty_selection = []
for i in range(player_count):
    empty_selection.append(None)

# Game state
deck = cards[:]
random.shuffle(deck)
players = [{"name": f"Player {i + 1}", "cards": [], "points": 0} for i in range(player_count)]
current_round = 1
selected_cards = []  # Tracks card selections for both players
for i in range(player_count):
    selected_cards.append(None)
game_over = False
round_results = []

# Distribute cards
for i in range(cards_per_player * player_count):
    players[i % player_count]["cards"].append(deck.pop())

# Language option
language = next((opt["value"] for opt in options if opt["name"] == "language"), "en")

# Language translations
translations = {
    "en": {"round": "Round", "points": "Points", "winner": "Winner", "select": "Select a card to play!"},
    "fr": {"round": "Tour", "points": "Points", "winner": "Gagnant", "select": "Sélectionnez une carte à jouer !"}
}
texts = translations.get(language, translations["en"])

# Fonts and Colors
font = pygame.font.Font(pygame.font.get_default_font(), 32)
big_font = pygame.font.Font(pygame.font.get_default_font(), 48)
player_font = pygame.font.Font(pygame.font.get_default_font(), 60)
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
CARD_COLOR = (255, 223, 186)  # Soft beige
CARD_BORDER_COLOR = (0, 0, 0)
GOLD = (218, 165, 32)
LIGHT_GRAY = (240, 240, 240)

# Helper Functions
def draw_background():
    screen.fill(WHITE)  # Set the background to white
    pygame.draw.rect(screen, LIGHT_GRAY, (0, 0, 1200, 100))  # Top bar
    title = big_font.render(f"{game_name}", True, BLACK)
    round_info = big_font.render(f"{texts['round']} {current_round}/{rounds}", True, BLACK)
    screen.blit(title, (50, 20))
    screen.blit(round_info, (900, 20))

def draw_players():
    x_offset = 50
    y_offset = 120
    for i, player in enumerate(players):
        color = BLACK if selected_cards[i] is None else GOLD
        player_text = player_font.render(f"{player['name']} ({player['points']} {texts['points']})", True, color)
        screen.blit(player_text, (x_offset, y_offset))
        x_offset += 0 if i % 2 == 0 else 600
        y_offset += 80 if i % 2 == 0 else -80

def draw_cards(player_index):
    x_start = 100 + (500 * (player_index // 2))
    y_start = 300 if player_index % 2 == 0 else 500
    player = players[player_index]

    size = (100, 150)

    for i, card in enumerate(player["cards"]):  # Display all cards for the player
        rect = pygame.Rect(x_start + i * 110, y_start, 100, 150)
        if card["bg"]["type"] == "color":

            # Convert the card's color from text to an RGB tuple
            card_color = {
                "red": (255, 0, 0),
                "black": (0, 0, 0),
                "blue": (0, 0, 255),
                "green": (0, 255, 0),
                "yellow": (255, 255, 0)
            }.get(card["bg"]["color"], CARD_COLOR)  # Default to CARD_COLOR if not specified

            pygame.draw.rect(screen, GOLD if selected_cards[player_index] == i else CARD_BORDER_COLOR, rect, 5)
            pygame.draw.rect(screen, card_color, rect.inflate(-10, -10), border_radius=15)  # Use card color here

            card_text = font.render(card["name"], True, WHITE if card["bg"]["color"] in ["black", "blue"] else BLACK)
            screen.blit(card_text, (x_start + i * 110 + 10, y_start + 60))
            card["rect"] = rect  # Store rect for click detection
        else:

            pygame.draw.rect(screen, GOLD if selected_cards[player_index] == i else CARD_BORDER_COLOR, rect, 5)
            # pygame.draw.rect(screen, card_color, rect.inflate(-10, -10), border_radius=15)  # Use card color here

            # Load images
            card_template = pygame.image.load(card["bg"]["path"])
            card_template = pygame.transform.scale(card_template, size)

            screen.blit(card_template, (x_start + i * 110 + 10, y_start + 60))

            card_text = font.render(card["name"], True, WHITE)
            screen.blit(card_text, (x_start + i * 110 + 10, y_start + 60))
            card["rect"] = rect  # Store rect for click detection


def draw_round_results():
    if round_results:
        result_text = big_font.render(f"Last Round: {round_results[-1]}", True, BLACK)
        pygame.draw.rect(screen, LIGHT_GRAY, (50, 700, 1100, 50))  # Background for results
        screen.blit(result_text, (70, 710))

def compare_cards(card1, card2):
    rule = rules[0]  # Apply the first rule
    if rule["winner"] == "higher-value":
        return card1["value"] > card2["value"]
    elif rule["winner"] == "lower-value":
        return card1["value"] < card2["value"]
    return False

def compare_cards2(cards):
    rule = rules[0]  # Apply the first rule
    if rule["winner"] == "higher-value":
        return max_indexes(cards)
    elif rule["winner"] == "lower-value":
        return min_indexes(cards)
    return []

def handle_round():
    global current_round, selected_cards, game_over

    if None not in selected_cards:  # Both players have selected their cards
        cards = [players[i]["cards"].pop(selected_cards[i]) for i in range(player_count)]

        winners = compare_cards2(cards)
        for w in winners:
            players[w]["points"] += 1
        
        # if compare_cards(cards):
        #     players[0]["points"] += 1
        #     round_results.append(f"{players[0]['name']} wins with {card1['name']}!")
        # elif compare_cards():
        #     players[1]["points"] += 1
        #     round_results.append(f"{players[1]['name']} wins with {card2['name']}!")
        # else:
        #     round_results.append("It's a tie!")

        # selected_cards = [None, None]  # Reset selections
        for i in range(len(selected_cards)):
            selected_cards[i] = None
        current_round += 1

        if current_round >= rounds or not players[0]["cards"] or not players[1]["cards"]:
            game_over = True

        for i in range(player_count):
            players[i]["cards"].append(deck.pop())

# Main Game Loop
running = True
while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False
        elif event.type == pygame.MOUSEBUTTONDOWN and not game_over:
            pos = pygame.mouse.get_pos()
            for i in range(player_count):
                if selected_cards[i] is None:  # Allow only unselected players to choose
                    for j, card in enumerate(players[i]["cards"]):
                        if "rect" in card and card["rect"].collidepoint(pos):
                            selected_cards[i] = j  # Record selection
                            break

    draw_background()
    draw_players()
    for i in range(player_count):  # Draw cards for all players
        draw_cards(i)
    draw_round_results()

    handle_round()

    if game_over:
        winner = max(players, key=lambda p: p["points"])
        winner_text = big_font.render(f"{texts['winner']}: {winner['name']}!", True, BLACK)
        screen.blit(winner_text, (400, 350))

    pygame.display.flip()

pygame.quit()
