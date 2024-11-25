import pygame
import sys

def create_card(name, description, image_path, stats):
    # Set up dimensions
    card_width, card_height = 600, 800
    card_surface = pygame.Surface((card_width, card_height))

    # Load images
    card_template = pygame.image.load("blue.png")
    card_template = pygame.transform.scale(card_template, (card_width, card_height))
    character_image = pygame.image.load(image_path)
    character_image = pygame.transform.scale(character_image, (300, 250))

    # Colors and fonts
    font = pygame.font.Font(None, 48)  # Larger font for the name
    small_font = pygame.font.Font(None, 30)  # Slightly larger font for better visibility
    black = (0, 0, 0)

    # Render text
    name_text = font.render(name, True, black)
    description_text = pygame.font.Font(None, 24).render(description, True, black)

    # Draw everything on card surface
    card_surface.blit(card_template, (0, 0))
    card_surface.blit(character_image, (150, 250))
    
    # Draw name on top (centered horizontally)
    name_text_rect = name_text.get_rect(center=(card_width // 2, 80))
    card_surface.blit(name_text, name_text_rect)

    # Draw description (centered horizontally under the image)
    description_text_rect = description_text.get_rect(center=(card_width // 2, 550))
    card_surface.blit(description_text, description_text_rect)

    # Draw stats
    y_offset = 520
    for stat_name, value in stats.items():
        stat_text = pygame.font.Font(None, 24).render(f"{stat_name}: {'*' * value}", True, black)
        card_surface.blit(stat_text, (20, y_offset))
        y_offset += 30

    return card_surface

def main():
    # Initialize Pygame
    pygame.init()

    # Set up screen dimensions
    screen_width, screen_height = 1200, 800
    screen = pygame.display.set_mode((screen_width, screen_height))
    pygame.display.set_caption("Card Creator")

    # Define card details
    cards = [
        {
            "name": "Electro Eevee",
            "description": "A powerful electric type creature with the ability to zap opponents.",
            "image_path": "eevee.png",
            "stats": {
                "Attack": 5,
                "Defense": 3,
                "Healing": 2,
                "Speed": 7,
                "Shielding": 3
            }
        },
        {
            "name": "Flare Fox",
            "description": "A fiery fox with a blazing spirit and high-speed agility.",
            "image_path": "bulbasaur.png",
            "stats": {
                "Attack": 6,
                "Defense": 4,
                "Healing": 1,
                "Speed": 8,
                "Shielding": 2
            }
        }
    ]

    # Create card surfaces
    card_surfaces = [create_card(card['name'], card['description'], card['image_path'], card['stats']) for card in cards]

    # Run until the user asks to quit
    running = True
    while running:
        # Handle events
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

        # Draw cards
        screen.fill((255, 255, 255))  # Fill the screen with white
        x_offset = 0
        for card_surface in card_surfaces:
            screen.blit(card_surface, (x_offset, 0))
            x_offset += 600

        # Update the display
        pygame.display.flip()

    # Quit Pygame
    pygame.quit()
    sys.exit()

if __name__ == "__main__":
    main()
