kin - A simple relational algebra system
========================================

kin is a system intended to evaluate relational algebra expressions, to help
people (specifically, me) develop their understanding of and intuitions around
relational algebra. It provides functions to load and store data into
relations and to evaluate expressions over those relations. It also provides a
parser allowing users to write relational algebra expressions in plain text
that roughly resemble the traditional notation. kin is not intended to be
directly useful as a data querying tool - in particular, it does not make any
attempt to represent data in an efficient way, or to perform queries
efficiently.

The text syntax
---------------

* P[attr1, attr2, ... attrn] (expr) -- projection
* S[(attr1 = 'val' AND attr2 = attr3) OR (attr4 = 'val')] (expr) -- selection
* R[oldname -> newname, othername->newername] (expr) -- rename
* expr U expr -- union
* expr N expr -- intersection
* expr - expr -- difference
* expr * expr -- cartesian product
* expr >< expr -- natural join
* expr / expr -- division 

TODO
----

 * A command-line that allows working with relations using the simplified text
   syntax
 * A pretty-printer that converts the simple text syntax into TeX math expressions 
                                    




