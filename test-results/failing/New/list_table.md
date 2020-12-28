# New - list_table

## Example undefined

This markdown:

````````````markdown
* Table in list:

  | column1 | column2 |
  |---------|---------|
  | value1  | value2  |
  | value3  | value4  |

* No leading pipe table in list:

  column1 | column2
  --------|--------
  value1  | value2
  value3  | value4

````````````

Should give output:

````````````html
<ul><li><p>Table in list:</p><table><thead><tr><th>column1</th><th>column2</th></tr></thead><tbody><tr><td>value1</td><td>value2</td></tr><tr><td>value3</td><td>value4</td></tr></tbody></table></li><li><p>No leading pipe table in list:</p><table><thead><tr><th>column1</th><th>column2</th></tr></thead><tbody><tr><td>value1</td><td>value2</td></tr><tr><td>value3</td><td>value4</td></tr></tbody></table></li></ul>
````````````

But instead was:

````````````html
<ul><li><p>Table in list:</p><p>| column1 | column2 | |---------|---------| | value1 | value2 | | value3 | value4 |</p><p>column1 | column2</p></li><li><p>No leading pipe table in list:</p></li></ul><p>--------|--------</p><p>value1 | value2 value3 | value4</p>
````````````
