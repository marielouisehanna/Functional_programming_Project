import pygame
import random

# Initialize Pygame
pygame.init()

# Screen dimensions
WINDOW_WIDTH, WINDOW_HEIGHT = 1000, 600
screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
pygame.display.set_caption("Creature Battle")

# Load background image
try:
    background = pygame.image.load("background.jpg")
    background = pygame.transform.smoothscale(background, (WINDOW_WIDTH, WINDOW_HEIGHT))  # Use smoothscale for better quality
except FileNotFoundError:
    print("Error: 'background.jpg' not found in the current directory.")
    print("Please add a 'background.jpg' file to the game folder.")
    exit()

# Fonts
font = pygame.font.Font(None, 36)

# Load creature images
try:
    creature_images = {
        "Pikachu": pygame.image.load("pikachu.png"),
        "Charmander": pygame.image.load("charmander.png"),
        "Bulbasaur": pygame.image.load("bulbasaur.png"),
        "Squirtle": pygame.image.load("squirtle.png"),
        "Eevee": pygame.image.load("eevee.png"),
        "ayle": pygame.image.load("ayle.png"),
    }
    # Scale creature images with smoothscale for better quality
    for name in creature_images:
        creature_images[name] = pygame.transform.smoothscale(creature_images[name], (100, 100))
except FileNotFoundError as e:
    print(f"Error: {e}. Make sure all creature images are in the directory.")
    exit()

# Load creatures
def load_creatures(file):
    creatures = {}
    with open(file, "r") as f:
        for line in f:
            parts = line.strip().split(",")
            name = parts[0]
            hp = int(parts[1])
            if len(parts) > 2:
                attack = (parts[2], int(parts[3]))
            else:
                attack = None
            creatures[name] = {"hp": hp, "max_hp": hp, "attack": attack, "buff": 0, "status": "normal"}
    return creatures

# Load turns
def load_turns(file):
    turns = []
    with open(file, "r") as f:
        for line in f:
            player, actions = line.strip().split(": ")
            actions_list = actions.split(" ")
            grouped_actions = [
                {"type": actions_list[i], "attacker": actions_list[i + 1], "target": actions_list[i + 3]}
                if actions_list[i] == "ATTACK"
                else {"type": actions_list[i], "target": actions_list[i + 1]}
                for i in range(0, len(actions_list), 4)
            ]
            turns.append({"player": player, "actions": grouped_actions})
    return turns

# Initialize game
creatures = load_creatures("creatures.txt")
turns = load_turns("turns.txt")

# Positions for creatures (aligned along the ground)
positions = {
    "Pikachu": (150, 450),
    "Charmander": (300, 450),
    "Bulbasaur": (450, 450),
    "Squirtle": (600, 450),
    "Eevee": (750, 450),
    "ayle": (900, 450),
}

# Draw creatures with images, health bars, names on top, and HP below names
def draw_creatures():
    screen.blit(background, (0, 0))  # Draw the background image
    for name, pos in positions.items():
        creature = creatures[name]
        if creature["hp"] > 0:  # Only draw the creature if it has HP left
            # Draw name on top of the creature, moved slightly higher
            name_text = font.render(name, True, (0, 0, 0))  # Create name text
            name_text_rect = name_text.get_rect(center=(pos[0], pos[1] - 110))  # Position further above the creature
            screen.blit(name_text, name_text_rect)  # Draw name text

            # Draw HP below the name, moved slightly higher
            hp_text = font.render(f"HP: {creature['hp']}", True, (0, 0, 0))  # Create HP text
            hp_text_rect = hp_text.get_rect(center=(pos[0], pos[1] - 80))  # Position just below the name
            screen.blit(hp_text, hp_text_rect)  # Draw HP text

            # Draw the creature's image
            image = creature_images[name]
            screen.blit(image, (pos[0] - 50, pos[1] - 50))  # Center image above the ground

        # Draw health bar
        bar_width = 100
        health_ratio = max(creature["hp"], 0) / creature["max_hp"]
        pygame.draw.rect(screen, (255, 0, 0), (pos[0] - 50, pos[1] + 60, bar_width, 10))  # Red background
        pygame.draw.rect(screen, (0, 255, 0), (pos[0] - 50, pos[1] + 60, bar_width * health_ratio, 10))  # Green overlay



# Execute actions
def execute_action(action):
    if action["type"] == "ATTACK":
        attacker = action["attacker"]
        target = action["target"]
        damage = creatures[attacker]["attack"][1] + creatures[attacker]["buff"]
        damage = random.randint(damage - 5, damage + 5)  # Randomize damage
        creatures[target]["hp"] -= damage
        print(f"{attacker} attacked {target} for {damage} damage!")
    elif action["type"] == "HEAL":
        target = action["target"]
        heal_amount = 20
        creatures[target]["hp"] += heal_amount
        if creatures[target]["hp"] > creatures[target]["max_hp"]:
            creatures[target]["hp"] = creatures[target]["max_hp"]
        print(f"{target} healed for {heal_amount} HP!")
    elif action["type"] == "BUFF":
        target = action["target"]
        creatures[target]["buff"] += 10
        print(f"{target} received a buff!")

# Main game loop
def main():
    clock = pygame.time.Clock()
    current_turn_index = 0
    action_index = 0
    action_timer = pygame.time.get_ticks()

    running = True
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

        # Process actions turn by turn
        if current_turn_index < len(turns):
            turn = turns[current_turn_index]
            if action_index < len(turn["actions"]):
                if pygame.time.get_ticks() - action_timer > 2000:  # 2-second delay
                    execute_action(turn["actions"][action_index])
                    action_index += 1
                    action_timer = pygame.time.get_ticks()
            else:
                action_index = 0
                current_turn_index += 1

        # Draw creatures
        draw_creatures()
        pygame.display.flip()
        clock.tick(60)

    pygame.quit()

if __name__ == "__main__":
    main()
