# GFM - List items

## [Example 224](https://spec.commonmark.org/0.29/#example-224)

This markdown:

````````````markdown
1.  A paragraph
    with two lines.

        indented code

    > A block quote.

````````````

Should give output:

````````````html
<ol><li><p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>
````````````

But instead was:

````````````html
<ol><li>A paragraph</li></ol><pre><code>with two lines.</code></pre><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
````````````
## [Example 228](https://spec.commonmark.org/0.29/#example-228)

This markdown:

````````````markdown
 -    one

      two

````````````

Should give output:

````````````html
<ul><li><p>one</p><p>two</p></li></ul>
````````````

But instead was:

````````````html
<ul><li>one</li></ul><pre><code>two</code></pre>
````````````
## [Example 229](https://spec.commonmark.org/0.29/#example-229)

This markdown:

````````````markdown
   > > 1.  one
>>
>>     two

````````````

Should give output:

````````````html
<blockquote><blockquote><ol><li><p>one</p><p>two</p></li></ol></blockquote></blockquote>
````````````

But instead was:

````````````html
<blockquote><blockquote><ol><li>one</li></ol><pre><code>two</code></pre></blockquote></blockquote>
````````````
## [Example 233](https://spec.commonmark.org/0.29/#example-233)

This markdown:

````````````markdown
1.  foo

    ```
    bar
    ```

    baz

    > bam

````````````

Should give output:

````````````html
<ol><li><p>foo</p><pre><code>bar</code></pre><p>baz</p><blockquote><p>bam</p></blockquote></li></ol>
````````````

But instead was:

````````````html
<ol><li>foo</li></ol><pre><code>``` bar ```</code></pre><pre><code>baz</code></pre><pre><code>&gt; bam</code></pre>
````````````
## [Example 234](https://spec.commonmark.org/0.29/#example-234)

This markdown:

````````````markdown
- Foo

      bar


      baz

````````````

Should give output:

````````````html
<ul><li><p>Foo</p><pre><code>bar baz</code></pre></li></ul>
````````````

But instead was:

````````````html
<ul><li>Foo</li></ul><pre><code>bar</code></pre><pre><code>baz</code></pre>
````````````
## [Example 240](https://spec.commonmark.org/0.29/#example-240)

This markdown:

````````````markdown
- foo

      bar

````````````

Should give output:

````````````html
<ul><li><p>foo</p><pre><code>bar</code></pre></li></ul>
````````````

But instead was:

````````````html
<ul><li>foo</li></ul><pre><code>bar</code></pre>
````````````
## [Example 241](https://spec.commonmark.org/0.29/#example-241)

This markdown:

````````````markdown
  10.  foo

           bar

````````````

Should give output:

````````````html
<ol start="10"><li><p>foo</p><pre><code>bar</code></pre></li></ol>
````````````

But instead was:

````````````html
<p>10. foo</p><pre><code>bar</code></pre>
````````````
## [Example 243](https://spec.commonmark.org/0.29/#example-243)

This markdown:

````````````markdown
1.     indented code

   paragraph

       more code

````````````

Should give output:

````````````html
<ol><li><pre><code>indented code</code></pre><p>paragraph</p><pre><code>more code</code></pre></li></ol>
````````````

But instead was:

````````````html
<ol><li>indented code</li></ol><p>paragraph</p><pre><code>more code</code></pre>
````````````
## [Example 244](https://spec.commonmark.org/0.29/#example-244)

This markdown:

````````````markdown
1.      indented code

   paragraph

       more code

````````````

Should give output:

````````````html
<ol><li><pre><code>indented code</code></pre><p>paragraph</p><pre><code>more code</code></pre></li></ol>
````````````

But instead was:

````````````html
<ol><li>indented code</li></ol><p>paragraph</p><pre><code>more code</code></pre>
````````````
## [Example 248](https://spec.commonmark.org/0.29/#example-248)

This markdown:

````````````markdown
-
  foo
-
  ```
  bar
  ```
-
      baz

````````````

Should give output:

````````````html
<ul><li>foo</li><li><pre><code>bar</code></pre></li><li><pre><code>baz</code></pre></li></ul>
````````````

But instead was:

````````````html
<ul><li>foo<code>bar</code>baz</li><li></li><li></li></ul>
````````````
## [Example 250](https://spec.commonmark.org/0.29/#example-250)

This markdown:

````````````markdown
-

  foo

````````````

Should give output:

````````````html
<ul><li></li></ul><p>foo</p>
````````````

But instead was:

````````````html
<ul><li><p>foo</p></li></ul>
````````````
## [Example 253](https://spec.commonmark.org/0.29/#example-253)

This markdown:

````````````markdown
1. foo
2.
3. bar

````````````

Should give output:

````````````html
<ol><li>foo</li><li></li><li>bar</li></ol>
````````````

But instead was:

````````````html
<ol><li>foo</li><li>bar</li></ol>
````````````
## [Example 256](https://spec.commonmark.org/0.29/#example-256)

This markdown:

````````````markdown
 1.  A paragraph
     with two lines.

         indented code

     > A block quote.

````````````

Should give output:

````````````html
<ol><li><p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>
````````````

But instead was:

````````````html
<p>1. A paragraph with two lines.</p><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
````````````
## [Example 257](https://spec.commonmark.org/0.29/#example-257)

This markdown:

````````````markdown
  1.  A paragraph
      with two lines.

          indented code

      > A block quote.

````````````

Should give output:

````````````html
<ol><li><p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>
````````````

But instead was:

````````````html
<p>1. A paragraph with two lines.</p><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
````````````
## [Example 258](https://spec.commonmark.org/0.29/#example-258)

This markdown:

````````````markdown
   1.  A paragraph
       with two lines.

           indented code

       > A block quote.

````````````

Should give output:

````````````html
<ol><li><p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>
````````````

But instead was:

````````````html
<p>1. A paragraph with two lines.</p><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
````````````
## [Example 259](https://spec.commonmark.org/0.29/#example-259)

This markdown:

````````````markdown
    1.  A paragraph
        with two lines.

            indented code

        > A block quote.

````````````

Should give output:

````````````html
<pre><code>1. A paragraph with two lines. indented code &gt; A block quote.</code></pre>
````````````

But instead was:

````````````html
<pre><code>1. A paragraph with two lines.</code></pre><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
````````````
## [Example 260](https://spec.commonmark.org/0.29/#example-260)

This markdown:

````````````markdown
  1.  A paragraph
with two lines.

          indented code

      > A block quote.

````````````

Should give output:

````````````html
<ol><li><p>A paragraph with two lines.</p><pre><code>indented code</code></pre><blockquote><p>A block quote.</p></blockquote></li></ol>
````````````

But instead was:

````````````html
<p>1. A paragraph with two lines.</p><pre><code>indented code</code></pre><pre><code>&gt; A block quote.</code></pre>
````````````
## [Example 261](https://spec.commonmark.org/0.29/#example-261)

This markdown:

````````````markdown
  1.  A paragraph
    with two lines.

````````````

Should give output:

````````````html
<ol><li>A paragraph with two lines.</li></ol>
````````````

But instead was:

````````````html
<p>1. A paragraph with two lines.</p>
````````````
## [Example 262](https://spec.commonmark.org/0.29/#example-262)

This markdown:

````````````markdown
> 1. > Blockquote
continued here.

````````````

Should give output:

````````````html
<blockquote><ol><li><blockquote><p>Blockquote continued here.</p></blockquote></li></ol></blockquote>
````````````

But instead was:

````````````html
<blockquote><ol><li><blockquote><p>Blockquote</p></blockquote></li></ol><p>continued here.</p></blockquote>
````````````
## [Example 263](https://spec.commonmark.org/0.29/#example-263)

This markdown:

````````````markdown
> 1. > Blockquote
> continued here.

````````````

Should give output:

````````````html
<blockquote><ol><li><blockquote><p>Blockquote continued here.</p></blockquote></li></ol></blockquote>
````````````

But instead was:

````````````html
<blockquote><ol><li><blockquote><p>Blockquote</p></blockquote></li></ol><p>continued here.</p></blockquote>
````````````
## [Example 264](https://spec.commonmark.org/0.29/#example-264)

This markdown:

````````````markdown
- foo
  - bar
    - baz
      - boo

````````````

Should give output:

````````````html
<ul><li>foo<ul><li>bar<ul><li>baz<ul><li>boo</li></ul></li></ul></li></ul></li></ul>
````````````

But instead was:

````````````html
<ul><li>foo<ul><li>baz<ul><li>boo</li></ul></li></ul></li><li>bar</li></ul>
````````````
## [Example 266](https://spec.commonmark.org/0.29/#example-266)

This markdown:

````````````markdown
10) foo
    - bar

````````````

Should give output:

````````````html
<ol start="10"><li>foo<ul><li>bar</li></ul></li></ol>
````````````

But instead was:

````````````html
<ol start="10"><li>foo</li></ol><pre><code>- bar</code></pre>
````````````
## [Example 270](https://spec.commonmark.org/0.29/#example-270)

This markdown:

````````````markdown
- # Foo
- Bar
  ---
  baz

````````````

Should give output:

````````````html
<ul><li><h1>Foo</h1></li><li><h2>Bar</h2>baz</li></ul>
````````````

But instead was:

````````````html
<ul><li><h1>Foo</h1></li><li>Bar</li></ul><hr><p>baz</p>
````````````
