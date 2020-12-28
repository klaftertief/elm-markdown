# New - list_item_text

## Example undefined

This markdown:

````````````markdown
  * item1

    * item2

  text

````````````

Should give output:

````````````html
<ul><li><p>item1</p><ul><li>item2</li></ul><p>text</p></li></ul>
````````````

But instead was:

````````````html
<ul><li>item1</li></ul><pre><code>* item2</code></pre><p>text</p>
````````````
