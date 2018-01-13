type globalWindow = {. [@bs.set] "onload": unit => unit };

type event = {. "which": int };

let key_left = 37;
let key_right = 39;

[@bs.val] external window : globalWindow = "window";
[@bs.val] external addEventListener : (string, (event) => unit) => unit = "document.addEventListener";

type status = 
    | Stopped 
    | Running 
    | GameOver;

type ball = {
    speed: int,
    mutable x: int,
    mutable y: int,
    mutable direction_x: int,
    mutable direction_y: int
};

type pong = {
    mutable status: status,
    mutable pressed_key: option(int),
    mutable score: int,
    ball: ball
};

let playground_width = (): int => [%bs.raw {| document.getElementById("playground").offsetWidth |}];
let playground_height = (): int => [%bs.raw {| document.getElementById("playground").offsetHeight |}];

let draw_end_game () = [%bs.raw {| document.getElementById("game-over").style.display = "block" |}];

let next_position (current_position, speed, direction) = current_position + speed * direction;

let move_ball_dir_x (game) = {
    let pos_x = next_position(game.ball.x, game.ball.speed, game.ball.direction_x);
    if (pos_x > playground_width()) {
        -1
    } else if (pos_x < 0) {
        1
    } else {
        game.ball.direction_x
    }
};

let move_ball_dir_y (game) = {
    let pos_y = next_position(game.ball.y, game.ball.speed, game.ball.direction_y);
    if (pos_y > playground_height()) {
        -1
    } else if (pos_y < 0) {
        1
    } else {
        game.ball.direction_y
    }
};

let move_ball_position (ball, direction) = ball.speed * direction;

let change_ball_position (game, dir_x, pos_x, dir_y, pos_y) = {
    game.ball.direction_x = dir_x;
    game.ball.direction_y = dir_y;
    game.ball.x = game.ball.x + pos_x;
    game.ball.y = game.ball.y + pos_y;
};

let draw_ball: (int, int) => unit = [%bs.raw 
    {| 
        function (x, y) {
            var ballHTML = document.getElementById('ball');
            ballHTML.style.left = x + 'px';
            ballHTML.style.top  = y + 'px';
        }
    |} 
];

let move_racket (racket_offset, game) = switch (game.pressed_key) {
| Some(n) => if (n == key_left) { racket_offset - 5 } else { racket_offset + 5 }
| None => racket_offset
};

let ball_offset_height = (): int => [%bs.raw {| document.getElementById('ball').offsetHeight |}];
let racket_offset_left = (): int => [%bs.raw {| document.getElementById('racket').offsetLeft |}];
let racket_offset_width = (): int => [%bs.raw {| document.getElementById('racket').offsetWidth |}];
let racket_offset_top = (): int => [%bs.raw {| document.getElementById('racket').offsetTop |}];
let racket_offset_height = (): int => [%bs.raw {| document.getElementById('racket').offsetHeight |}];
let racket_border_right = () => racket_offset_left() + racket_offset_width();
let racket_pos_y = () => racket_offset_top() - ball_offset_height() / 2;

let is_racket_hit (ball) = {
    let pos_x = next_position(ball.x, ball.speed, ball.direction_x);
    let pos_y = next_position(ball.y, ball.speed, ball.direction_y);
    pos_x >= racket_offset_left() &&
    pos_x <= racket_border_right() &&
    pos_y >= racket_pos_y();
};

let compute_score (ball, score) = if (is_racket_hit(ball)) { score + 1 } else { score };

let draw_racket: (int) => unit = [%bs.raw 
    {| 
        function (pixelPos) {
            document.getElementById('racket').style.left = pixelPos + 'px';
        }
    |} 
];

let draw_score: (int) => unit = [%bs.raw 
{| 
    function (score) {
        document.getElementById('score').innerHTML = score;
    }
|} 
];

let change_direction_y (ball) = if (is_racket_hit(ball)) { ball.direction_y = -1 };

let is_game_over (ball) = {
    let pos_y = next_position(ball.y, ball.speed, ball.direction_y) - racket_offset_height();
    pos_y > racket_pos_y();
};

let end_game (game) = if (is_game_over(game.ball)) { game.status = GameOver };

let run (game) = {
    let new_dir_x: int = move_ball_dir_x(game);
    let new_dir_y: int = move_ball_dir_y(game);
    let new_pos_x: int = move_ball_position(game.ball, new_dir_x);
    let new_pos_y: int = move_ball_position(game.ball, new_dir_y);
    change_ball_position(game, new_dir_x, new_pos_x, new_dir_y, new_pos_y);
    draw_ball(game.ball.x, game.ball.y);
    let pixel_pos = move_racket(racket_offset_left(), game);
    draw_racket(pixel_pos);
    game.score = compute_score(game.ball, game.score);
    draw_score(game.score);
    change_direction_y(game.ball);
    end_game(game);
};
    
let loop (game: pong) = 
    switch (game.status) {
    | Stopped => ()
    | GameOver => draw_end_game()
    | Running => run(game)
    };

let start (game) = {
    game.status = Running;
    [%bs.raw {| document.getElementById('start-message').style.display = "none" |}];
};

let set_pressed_key (game, key_number) = game.pressed_key = Some(key_number);

let clear_pressed_key (game) = game.pressed_key = None;

window##onload #= (() => {
    let game = {
        status: Stopped, 
        pressed_key: None,
        score: 0,
        ball: {
            speed: 5,
            x: 135,
            y: 100,
            direction_x: -1,
            direction_y: -1
        }
    };
    
    let _ = Js.Global.setInterval(() => loop(game), 16); 

    let () = addEventListener("keydown", (evt: event) => switch (game.status) {
    | Stopped => start(game)
    | Running => set_pressed_key(game, evt##which)
    | _ => ()
    });

    let () = addEventListener("keyup", (_) => clear_pressed_key(game));
});
