# GFM - Thematic breaks

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
