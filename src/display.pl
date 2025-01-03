/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

/*
   This file contains a list predicates aimed to aid with the display of information.
 */

% put_string(+String)
put_string(String) :-
        member(C, String),
        put_code(C),
        fail.
put_string(_).