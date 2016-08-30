elm-rest

the api could have an interesting thing that hooks into the schema api and can
generate toast messages if the queries or resources don’t make sense based on the schema of the tables? that could be pretty cool? kinda like a dev mode thing?

this might be better than returning results? the reason for this is that the user of the api would never really want to have to deal with results themselves… that is almost like modeling runtime errors… not something that we want. it’s interesting that server interaction kinda can generate run time errors if you don’t compose the right query… hmm. what is the solution to this problem?

client and server need to speak the same language… hmm… i think a dev mode is a good start of the lib. i think that the elm-css might do this. we should look into it.

https://github.com/elm-lang/elm-package
http://package.elm-lang.org/help/documentation-format
http://package.elm-lang.org/help/design-guidelines#avoid-gratuitous-abstraction

put the custom private type stuff in its own library
