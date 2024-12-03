import pygame
import json

# Initialisation de Pygame
pygame.init()
screen = pygame.display.set_mode((800, 600))
pygame.display.set_caption("Card Game")

# Chargement des données JSON
with open("game.json", "r") as f:
    game_data = json.load(f)

# Extraire les cartes, règles et options
cards = game_data["cards"]
rules = game_data["rules"]
options = game_data["options"]

# Obtenir les options configurées
language = next((opt["value"] for opt in options if opt["name"] == "language"), "en")
max_rounds = int(next((opt["value"] for opt in options if opt["name"] == "max-rounds"), 5))

# Traductions
translations = {
    "en": {"game": "Game", "rounds": "Rounds", "winner": "Winner"},
    "fr": {"game": "Jeu", "rounds": "Tours", "winner": "Gagnant"}
}
texts = translations.get(language, translations["en"])

# Couleurs
BLACK = (0, 0, 0)
RED = (255, 0, 0)
WHITE = (255, 255, 255)

# Initialiser les cartes
card_width = 100
card_height = 150
card_spacing = 20
start_x = (800 - (len(cards) * (card_width + card_spacing))) // 2
start_y = 200
card_rects = []

for i, card in enumerate(cards):
    x = start_x + i * (card_width + card_spacing)
    y = start_y
    card_rects.append(pygame.Rect(x, y, card_width, card_height))

# Fonction pour dessiner les cartes
def draw_cards(selected_card=None):
    screen.fill(BLACK)
    font = pygame.font.Font(None, 36)
    for i, rect in enumerate(card_rects):
        pygame.draw.rect(screen, RED, rect)
        text = font.render(cards[i]["name"], True, WHITE)
        screen.blit(text, (rect.x + 10, rect.y + 60))
        # Si une carte est sélectionnée, changer sa couleur
        if selected_card == i:
            pygame.draw.rect(screen, WHITE, rect, 3)
    pygame.display.flip()

# Fonction pour comparer les cartes
def compare_cards(card1, card2):
    value1 = card1["value"]
    value2 = card2["value"]
    if value1 > value2:
        return 0  # Le joueur 1 gagne
    elif value1 < value2:
        return 1  # Le joueur 2 gagne
    return -1  # Égalité

# Boucle principale
running = True
rounds = 0
selected_card = None
while running and rounds < max_rounds:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False
        elif event.type == pygame.MOUSEBUTTONDOWN:
            pos = pygame.mouse.get_pos()
            # Vérifiez si une carte a été cliquée
            for i, rect in enumerate(card_rects):
                if rect.collidepoint(pos):
                    selected_card = i

    draw_cards(selected_card)

    # Si une carte est sélectionnée, jouer un tour
    if selected_card is not None:
        # Comparer avec une carte aléatoire (par exemple, carte du "joueur 2")
        import random
        opponent_card = random.choice(cards)
        winner = compare_cards(cards[selected_card], opponent_card)

        # Afficher le gagnant
        font = pygame.font.Font(None, 48)
        screen.fill(BLACK)
        text = font.render(f"{texts['winner']}: Player {winner + 1 if winner >= 0 else 'Tie'}", True, WHITE)
        screen.blit(text, (200, 300))
        pygame.display.flip()

        pygame.time.wait(2000)  # Attendre avant le prochain tour
        selected_card = None
        rounds += 1

pygame.quit()
