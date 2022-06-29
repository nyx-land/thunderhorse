# Thunderhorse - An Org-Mode parser for Common Lisp
> Thunder... Thunder... Horse... Horse...

This is a work-in-progress Org-Mode parser for Common Lisp. The goal is to
eventually be compliant with the core markup functionality specified by
[orgdown](https://karl-voit.at/2021/11/27/orgdown/), and then also whatever
additional features I personally use Org-Mode for (especially the metadata
features like tags). This makes extensive use of CLOS to return the AST of an
Org-Mode document as a standard object so that you can do whatever you want with
it.

I don't recommend even trying to use this right now or looking at the code for
that matter.

## Why?

There are two Org-Mode parsers for CL currently. Both are incomplete and for
different reasons it seemed more productive to just implement one myself instead
of trying to figure out how to finish the other two.

## License

ISC. Do whatever you want with it, just make sure to credit me.
