import os
import sys
import pygame

from pyswip import Prolog
from utils import rotate
from date import (
    PROLOG_PATH, FPS, WIN, FONT,
    MAP, LIGHT, WIDTH, HEIGHT, POSITIONS,
    HUNTER_IDLE, HUNTER_MOVE, HUNTER_SHOOT,
    WUMPUS_IDLE, WUMPUS_BLOOD, GOLD, PIT,
    W_BS, W_BREEZE, W_STENCH, W_GOLD, EXIT
)

prolog = Prolog()
prolog.consult(PROLOG_PATH)

IS_AGENT = True


class element:
    def __init__(self, y, x):
        self.x, self.y = POSITIONS[x-1][y-1]

    def update(self):
        pass

    def draw(self):
        self.rect = self.image.get_rect(center=(self.x, self.y))
        WIN.blit(self.image, self.rect)


class Light():
    def __init__(self):
        self.x, self.y = 0, 0
        self.image = LIGHT
        self.filter = pygame.surface.Surface((WIDTH, HEIGHT))
        self.filter.fill(pygame.color.Color('white'))
        self.visited = []

    def update(self, hunter_obj):
        new_coords = (hunter_obj.x, hunter_obj.y)
        if new_coords not in self.visited and new_coords != (self.x, self.y):
            self.x, self.y = new_coords
            light_pos = (self.x, self.y)
            self.rect = self.image.get_rect(center=light_pos)
            self.visited.append(new_coords)
        else:
            self.rect = pygame.Rect(-999, -999, 0, 0)

    def draw(self):
        self.filter.blit(self.image, self.rect)
        WIN.blit(self.filter, (0, 0), special_flags=pygame.BLEND_RGBA_SUB)


class Hunter(element, pygame.sprite.Sprite):
    def __init__(self, x, y, orientation):
        element.__init__(self, x, y)
        pygame.sprite.Sprite.__init__(self)
        self.rotation = orientation
        self.sprites = {
            'idle': HUNTER_IDLE,
            'move': HUNTER_MOVE,
            'shoot': HUNTER_SHOOT
        }
        self.anim_speed = 1
        self.current_sprite = 0
        self.current_state = 'idle'
        self.image = self.sprites[self.current_state][self.current_sprite]

        self.rect = self.image.get_rect()
        self.rect.center = (self.x, self.y)

        self.facing_angle = {'right': 0, 'up': 90, 'left': 180, 'down': -90}

    def move(self, y, x, facing):
        self.rotation = facing
        self.image = rotate(self.image, self.facing_angle[facing])
        self.x, self.y = POSITIONS[x-1][y-1]

        self.rect = self.image.get_rect()
        self.rect.center = (self.x, self.y)

    def update(self):
        self.current_sprite += self.anim_speed
        if self.current_sprite >= len(self.sprites[self.current_state]):
            self.current_sprite = 0
        self.image = self.sprites[self.current_state][int(self.current_sprite)]

        new_pos = list(prolog.query("w_hunter( X, Y, Z)."))[0].values()
        self.move(*new_pos)


class Wumpus(element, pygame.sprite.Sprite):
    def __init__(self, x, y):
        element.__init__(self, x, y)
        pygame.sprite.Sprite.__init__(self)
        self.sprites = {
            'idle': WUMPUS_IDLE,
            'died': WUMPUS_BLOOD
        }
        self.anim_speed = 1
        self.current_sprite = 0
        self.current_state = 'idle'
        self.image = self.sprites[self.current_state][self.current_sprite]

        self.rect = self.image.get_rect()
        self.rect.center = (self.x, self.y)

    def update(self):
        if not list(prolog.query('w_wumpus(X,Y).')) and self.x != self.y != -999:
            self.x, self.y = (-999, -999)

        self.current_sprite += self.anim_speed
        if self.current_sprite >= len(self.sprites[self.current_state]):
            self.current_sprite = 0
        self.image = self.sprites[self.current_state][int(self.current_sprite)]

        self.rect = self.image.get_rect()
        self.rect.center = (self.x, self.y)


class Pit(element, pygame.sprite.Sprite):
    def __init__(self, x, y):
        super().__init__(x, y)
        self.image = PIT


class Gold(element, pygame.sprite.Sprite):
    def __init__(self, x, y):
        super().__init__(x, y)
        self.image = GOLD

    def update(self):
        if not list(prolog.query('w_gold(X,Y).')) and self.x != self.y != -999:
            self.x, self.y = (-999, -999)

    def draw(self):
        self.rect = self.image.get_rect(center=(self.x, self.y))
        WIN.blit(self.image, self.rect)

        if self.x == self.y == -999:
            WIN.blit(EXIT, (0, 0))


def update_objects(light, sprites, *elems):
    """update the car moves"""
    sprites.update()
    [el.update() for el in elems]
    light.update(sprites.sprites()[0])


def game_over():

    hunter_pos = list(list(prolog.query("w_hunter(X,Y,_)."))[0].values())
    query_params = f'({hunter_pos[0]},{hunter_pos[1]})'
    wumpus_hit = list(prolog.query(f'w_wumpus{query_params}.')) == [{}]
    pit_fall = list(prolog.query(f'w_pit{query_params}.')) == [{}]

    if wumpus_hit or pit_fall:
        score = list(prolog.query("h_score(X)"))[0]['X']
        if wumpus_hit:
            text = 'GAME OVER: Wumpus killed you!'
        elif pit_fall:
            text = 'GAME OVER: You fell into a pit!'

        font = pygame.font.Font(FONT, 30)
        text_surface = font.render(text, True, pygame.color.Color('white'))
        text_rect = text_surface.get_rect()
        text_rect.center = (WIDTH/2, HEIGHT/3)
        WIN.blit(text_surface, text_rect)

        text = 'Your score: {} point(s).'.format(score)
        text_surface = font.render(text, True, pygame.color.Color('white'))
        text_rect = text_surface.get_rect()
        text_rect.center = (WIDTH/2, HEIGHT/3 + 40)
        WIN.blit(text_surface, text_rect)

        pygame.display.update()
        while True:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    sys.exit()


def draw_sensors(hunter_obj):
    x, y = hunter_obj.x + 20, hunter_obj.y - 75

    sensors = list(prolog.query("getSensors(X)."))[0]['X']
    stench, breeze, glitter = sensors
    if glitter:
        WIN.blit(W_GOLD, (x, y))
    elif stench and breeze:
        WIN.blit(W_BS, (x, y))
    elif stench:
        WIN.blit(W_STENCH, (x, y))
    elif breeze:
        WIN.blit(W_BREEZE, (x, y))

    game_over()  # check game over


def draw_window(light, sprites, *elems):
    """update the window"""
    WIN.blit(MAP, (0, 0))
    [el.draw() for el in elems]
    sprites.draw(WIN)
    light.draw()
    draw_sensors(sprites.sprites()[0])
    pygame.display.update()


def valid_move(hunter_obj):
    facing = hunter_obj.rotation
    x, y = hunter_obj.x, hunter_obj.y

    if (
        (facing == 'right' and x == 695) or
        (facing == 'left' and x == 107) or
        (facing == 'up' and y == 107) or
        (facing == 'down' and y == 670)
    ):
        return False
    return True


def winner():
    text = 'WINNER: You managed to get the gold out!'

    font = pygame.font.Font(FONT, 30)
    text_surface = font.render(text, True, pygame.color.Color('white'))
    text_rect = text_surface.get_rect()
    text_rect.center = (WIDTH/2, HEIGHT/3)
    WIN.blit(text_surface, text_rect)

    score = list(prolog.query("h_score(X)"))[0]['X']
    text = 'Your score: {} point(s).'.format(score)
    text_surface = font.render(text, True, pygame.color.Color('white'))
    text_rect = text_surface.get_rect()
    text_rect.center = (WIDTH/2, HEIGHT/3 + 40)
    WIN.blit(text_surface, text_rect)

    pygame.display.update()
    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                sys.exit()


def user_controller(event, hunter, wumpus, gold):
    if event.key == pygame.K_UP and valid_move(hunter):
        list(prolog.query("move."))
    if event.key == pygame.K_LEFT:
        list(prolog.query("left."))
    if event.key == pygame.K_RIGHT:
        list(prolog.query("right."))
    if event.key == pygame.K_s:
        list(prolog.query("shoot."))
    if event.key == pygame.K_g:
        if list(list(prolog.query('w_hunter(X,Y,_), w_gold(X,Y), w_goal(0).'))):
            list(prolog.query("grab(0)."))
    if event.key == pygame.K_c:
        if list(list(prolog.query('w_hunter(1,1,_), w_goal(1)'))):
            list(prolog.query("climb(1)."))
            winner()


def main() -> None:
    """Main function to run the simulation."""

    list(prolog.query("run(pygame)."))

    last = pygame.time.get_ticks()
    cooldown = FPS * 6

    hunter = Hunter(*list(prolog.query("w_hunter( X, Y, Z)."))[0].values())
    wumpus = Wumpus(*list(prolog.query("w_wumpus( X, Y)."))[0].values())
    light = Light()

    moving_sprites = pygame.sprite.Group()
    moving_sprites.add(hunter)
    moving_sprites.add(wumpus)

    gold = Gold(*list(prolog.query("w_gold( X, Y)."))[0].values())
    pit_values = list(prolog.query("w_pit( X, Y)."))
    pits = [Pit(*pit_val.values()) for pit_val in pit_values]

    clock = pygame.time.Clock()
    while True:
        clock.tick(FPS)
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                sys.exit()
            if event.type == pygame.MOUSEBUTTONDOWN:
                print(pygame.mouse.get_pos())
            if event.type == pygame.KEYDOWN:
                if not IS_AGENT:
                    user_controller(event, hunter, wumpus, gold)

        update_objects(light, moving_sprites, *pits, gold)
        draw_window(light, moving_sprites, *pits, gold)

        if list(prolog.query("w_hunter(1,1,_), w_goal(1).")):
            winner()

        if IS_AGENT:
            now = pygame.time.get_ticks()
            if now - last >= cooldown:
                last = now
                list(prolog.query("runloop(-1)."))


if __name__ == '__main__':
    main()  # run main function
