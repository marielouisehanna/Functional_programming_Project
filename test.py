import json
import pygame
import sys

from pygame import MOUSEBUTTONDOWN

# to continue tmrow, sleep time, write function to generate code from json object of a line, and probably make it so the
# line where the code is written is based on the type of instruction the line is

def generate_code():
    pass


class Card:
    width = 80
    height = 160
    window_width = 60
    window_height = 40
    window_pos = (15,25)
    screen = []
    def __init__(self, title, background, img, stats):
        self.title = title
        self.background = background
        self.img = img
        self.stats = stats
        self.position = (-1000,-1000)


    def draw(self, scale=1):
        size = (Card.width*scale, Card.height*scale)
        window_size = (Card.window_width*scale, Card.window_height*scale)

        card_surface = pygame.Surface(size)

        # Load images
        card_template = pygame.image.load(self.background)
        card_template = pygame.transform.scale(card_template, size)
        character_image = pygame.image.load(self.img)
        character_image = pygame.transform.scale(character_image, window_size)

        # Colors and fonts
        font = pygame.font.Font(None, 12*scale)  # Larger font for the name
        small_font = pygame.font.Font(None, 30)
        black = (0, 0, 0)

        # Render text
        title_text = font.render(self.title, True, black)
        # description_text = pygame.font.Font(None, 24).render(description, True, black)

        # Draw everything on card surface
        card_surface.blit(card_template, (0, 0))
        scaled_window_pos = (Card.window_pos[0]*scale, Card.window_pos[1]*scale)
        card_surface.blit(character_image, scaled_window_pos)

        # Draw name on top (centered horizontally)
        name_text_rect = title_text.get_rect(center=((Card.width*scale // 2)-(5*scale), 10*scale))
        card_surface.blit(title_text, name_text_rect)



        return card_surface

    def set_position(self, x, y):
        self.position = (x, y)

    def display(self, x, y, scale=1):
        Card.screen.blit(self.draw(scale), (x, y))


class SubCard(Card):
    def __init__(self, title, background, img, stats):
        Card.__init__(self, title, background, img, stats)

    def sub_func1(self):
        pass

    def sub_func2(self, arg):
        pass


class Player:
    def __init__(self):
        pass

def in_interval(var, a, b):
    return (var>a) and (var<b)

def get_card_at_position(cards, x, y):
    horizontal_margin = Card.width
    vertical_margin = Card.height
    for card in cards:
        card_x = card.position[0]
        card_y = card.position[1]
        left_bound = card_x
        right_bound = card_x+horizontal_margin
        lower_bound = card_y
        upper_bound = card_y+vertical_margin
        if in_interval(x, left_bound, right_bound) and in_interval(y, lower_bound, upper_bound):
            return card
    return None



def main():
    # Initialize Pygame
    pygame.init()

    # Set up screen dimensions
    screen_width, screen_height = 1200, 800
    screen = pygame.display.set_mode((screen_width, screen_height))
    Card.screen = screen
    pygame.display.set_caption("Card Creator")

    # Define card details
    card_flareon = Card("Flareon", "cards/red.png", "cards/flareon.png", {"hp":80})
    card_jolteon = Card("Jolteon", "cards/yellow.png", "cards/jolteon.png", {"hp": 60})

    cards = [card_flareon, card_jolteon]


    # Run until the user asks to quit
    running = True
    while running:
        # Handle events
        for event in pygame.event.get():
            if event.type == MOUSEBUTTONDOWN:
                mouse_pos = pygame.mouse.get_pos()
                clicked_card = get_card_at_position(cards, mouse_pos[0], mouse_pos[1])
                if clicked_card is not None:
                    print(clicked_card.title)
                    display_loop = True
                    while display_loop:
                        clicked_card.display((screen_width-4*Card.width)//2, (screen_height-4*Card.height)//2, 4)
                        pygame.display.flip()
                        for event_ in pygame.event.get():
                            if event_.type == MOUSEBUTTONDOWN:
                                display_loop = False

            if event.type == pygame.QUIT:
                running = False

        # Draw cards
        screen.fill((255, 255, 255))  # Fill the screen with white

        x_offset = 0
        padding = 10
        for card in cards:
            card.set_position(x_offset, screen_height-Card.height)
            card.display(x_offset, screen_height-Card.height)
            x_offset += Card.width + padding

        # Update the display
        pygame.display.flip()

    # Quit Pygame
    pygame.quit()
    sys.exit()

if __name__ == "__main__":
    main()
