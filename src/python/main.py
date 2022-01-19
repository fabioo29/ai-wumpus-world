import os
import sys
import pygame

from pyswip import Prolog
from date import (
    PROLOG_PATH, FPS, WIN, FONT,
    MAP, LIGHT, WIDTH, HEIGHT, POSITIONS,
    HUNTER_IDLE, HUNTER_MOVE, HUNTER_SHOOT,
    WUMPUS_IDLE, WUMPUS_BLOOD, GOLD, PIT,
)


class element:
    def __init__(self, x, y):
        self.x, self.y = POSITIONS[x-1][y-1]

    def update(self):
        pass

    def draw(self):
        # draw self.image with (self.x,self.y) as center coords
        self.rect = self.image.get_rect(center=(self.x, self.y))
        WIN.blit(self.image, self.rect)


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

    def update(self):
        self.current_sprite += self.anim_speed
        if self.current_sprite >= len(self.sprites[self.current_state]):
            self.current_sprite = 0
        self.image = self.sprites[self.current_state][int(self.current_sprite)]


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
        self.current_sprite += self.anim_speed
        if self.current_sprite >= len(self.sprites[self.current_state]):
            self.current_sprite = 0
        self.image = self.sprites[self.current_state][int(self.current_sprite)]


class Pit(element, pygame.sprite.Sprite):
    def __init__(self, x, y):
        super().__init__(x, y)
        self.image = PIT


class Gold(element, pygame.sprite.Sprite):
    def __init__(self, x, y):
        super().__init__(x, y)
        self.image = GOLD


def update_objects(sprites, *elems):
    """update the car moves"""
    sprites.update()
    [el.update() for el in elems]


def draw_window(sprites, *elems):
    """update the window"""
    WIN.blit(MAP, (0, 0))
    # filter = pygame.surface.Surface((WIDTH, HEIGHT))
    # filter.fill(pygame.color.Color('Grey'))
    # light_pos = pygame.mouse.get_pos()
    # light_pos = (light_pos[0] - LIGHT.get_width()//2, light_pos[1] - LIGHT.get_height()//2)
    # filter.blit(LIGHT, light_pos)
    # WIN.blit(filter, (0, 0), special_flags=pygame.BLEND_RGBA_SUB)

    sprites.draw(WIN)
    [el.draw() for el in elems]


def main() -> None:
    """Main function to run the simulation."""

    prolog = Prolog()
    prolog.consult(PROLOG_PATH)
    list(prolog.query("run(agent)."))

    hunter = Hunter(*list(prolog.query("w_hunter( X, Y, Z)."))[0].values())
    wumpus = Wumpus(*list(prolog.query("w_wumpus( X, Y)."))[0].values())

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

        update_objects(moving_sprites, *pits, gold)
        draw_window(moving_sprites, *pits, gold)
        pygame.display.update()


if __name__ == '__main__':
    main()  # run main function
