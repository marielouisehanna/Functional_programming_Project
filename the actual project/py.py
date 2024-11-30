import json

class CardGame:
    def __init__(self, game_data):
        self.game_name = game_data['gameName']
        self.num_players = game_data['numPlayers']
        self.cards = game_data['cards']
        self.rules = game_data['rules']
        self.actions = game_data['actions']

    def start_game(self):
        print(f"Starting the game: {self.game_name}")
        print(f"Number of players: {self.num_players}")
        print(f"Cards: {self.cards}")
        print(f"Rules: {self.rules}")
        print(f"Actions: {self.actions}")
        # Game logic goes here

if __name__ == "__main__":
    with open('game.json', 'r') as f:
        game_data = json.load(f)
    game = CardGame(game_data)
    game.start_game()
