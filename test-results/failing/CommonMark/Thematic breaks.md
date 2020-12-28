# CommonMark - Thematic breaks

## [Example 21](https://spec.commonmark.org/0.29/#example-21)

This markdown:

````````````markdown
 - - -

````````````

Should give output:

````````````html
<hr>
````````````

But instead was:

````````````html
<ul><li><ul><li>-</li></ul></li></ul>
````````````
## [Example 23](https://spec.commonmark.org/0.29/#example-23)

This markdown:

````````````markdown
-     -      -      -

````````````

Should give output:

````````````html
<hr>
````````````

But instead was:

````````````html
<ul><li><ul><li><ul><li>-</li></ul></li></ul></li></ul>
````````````
## [Example 24](https://spec.commonmark.org/0.29/#example-24)

This markdown:

````````````markdown
- - - -    

````````````

Should give output:

````````````html
<hr>
````````````

But instead was:

````````````html
<ul><li><ul><li><ul><li><ul><li></li></ul></li></ul></li></ul></li></ul>
````````````
## [Example 30](https://spec.commonmark.org/0.29/#example-30)

This markdown:

````````````markdown
* Foo
* * *
* Bar

````````````

Should give output:

````````````html
<ul><li>Foo</li></ul><hr><ul><li>Bar</li></ul>
````````````

But instead was:

````````````html
<ul><li>Foo</li><li><ul><li>*</li></ul></li><li>Bar</li></ul>
````````````
## [Example 31](https://spec.commonmark.org/0.29/#example-31)

This markdown:

````````````markdown
- Foo
- * * *

````````````

Should give output:

````````````html
<ul><li>Foo</li><li><hr></li></ul>
````````````

But instead was:

````````````html
<ul><li>Foo</li><li><ul><li><ul><li>*</li></ul></li></ul></li></ul>
````````````
