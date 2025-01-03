/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- include('menu.pl').

play:-
    menu(0, Mode, Players, Difficulty). % Singleton variables to be filled by the user