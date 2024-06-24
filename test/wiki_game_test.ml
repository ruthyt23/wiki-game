open! Core
open! Expect_test_helpers_core
open! Wiki_game_lib

let%expect_test "get_first_item_of_all_unordered_lists" =
  let contents =
    {|<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta http-equiv="X-UA-Compatible" content="ie=edge">
        <title>My Blog</title>
        <link rel="stylesheet" href="style.css">
    </head>
    <body>
      <ul>
        <li>apple</li>
        <li>orange</li>
        <li>banana</li>
      </ul>
      <ul>
        <li>watermelon</li>
        <li>kiwi</li>
        <li>grapes</li>
      </ul>
      <ul>
        <li>pineapple</li>
        <li>strawberry</li>
        <li>peach</li>
      </ul>
    </body>
    <script src="index.js"></script>
</html>
|}
  in
  List.iter
    (Lambda_soup_utilities.get_first_item_of_all_unordered_lists contents)
    ~f:(fun x -> print_endline x);
  [%expect {| 
  apple
  watermelon
  pineapple
  |}]
;;

let%expect_test "get_first_item_of_second_unordered_list" =
  let contents =
    {|<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta http-equiv="X-UA-Compatible" content="ie=edge">
        <title>My Blog</title>
        <link rel="stylesheet" href="style.css">
    </head>
    <body>
      <ul>
        <li>apple</li>
        <li>orange</li>
        <li>banana</li>
      </ul>
      <ul>
        <li>watermelon</li>
        <li>kiwi</li>
        <li>grapes</li>
      </ul>
      <ul>
        <li>pineapple</li>
        <li>strawberry</li>
        <li>peach</li>
      </ul>
    </body>
    <script src="index.js"></script>
</html>
|}
  in
  print_endline
    (Lambda_soup_utilities.get_first_item_of_second_unordered_list contents);
  [%expect {| 
  watermelon
  |}]
;;

let%expect_test "get_bolded_text" =
  let contents =
    {|<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta http-equiv="X-UA-Compatible" content="ie=edge">
        <title>My Blog</title>
        <link rel="stylesheet" href="style.css">
    </head>
    <body>
      <ul>
        <li>apple</li>
        <li>orange</li>
        <li><b>banana</b></li>
      </ul>
      <ul>
        <li>watermelon</li>
        <li><b>kiwi</b></li>
        <li>grapes</li>
      </ul>
      <ul>
        <li>pineapple</li>
        <li><b>strawberry</b></li>
        <li>peach</li>
      </ul>
    </body>
    <script src="index.js"></script>
</html>
|}
  in
  List.iter (Lambda_soup_utilities.get_bolded_text contents) ~f:(fun x ->
    print_endline x);
  [%expect {| 
banana
kiwi
strawberry
|}]
;;

let%expect_test "get_linked_articles" =
  let contents =
    {|<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta http-equiv="X-UA-Compatible" content="ie=edge">
        <title>My Blog</title>
        <link rel="stylesheet" href="style.css">
    </head>
    <body>
      <ul>
        <li><a href='/wiki/Talk:Apple'>apple</li>
        <li>orange</li>
        <li><a href='/wiki/Banana'>banana</li>
      </ul>
      <ul>
        <li>watermelon</li>
        <li>kiwi</li>
        <li>grapes</li>
      </ul>
      <ul>
        <li>pineapple</li>
        <li><a href='/wiki/Strawberry'>strawberry</li>
        <li>peach</li>
      </ul>
    </body>
    <script src="index.js"></script>
</html>
|}
  in
  List.iter (Wiki_game.get_linked_articles contents) ~f:(fun x ->
    print_endline x);
  [%expect {| 
/wiki/Banana
/wiki/Strawberry
|}]
;;

let%expect_test "get_credits" =
  let contents =
    {|<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta http-equiv="X-UA-Compatible" content="ie=edge">
        <title>My Blog</title>
        <link rel="stylesheet" href="style.css">
    </head>
    <body>
      <ul>
        <li><a class='ipc-primary-image-list-card__title'>Apple</a></li>
        <li>Orange</li>
        <li><a class='ipc-primary-image-list-card__title'>Banana</a></li>
        <li>Kiwi</li>
        <li>Strawberry</li>
        <li><a class='ipc-primary-image-list-card__title'>Mango</a></li>
      </ul>
    </body>
    <script src="index.js"></script>
</html>
|}
  in
  List.iter (Imdb.get_credits contents) ~f:(fun x -> print_endline x);
  [%expect {| 
Apple
Banana
Mango
|}]
;;
