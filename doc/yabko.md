

# Module yabko #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-int64">int64()</a> ###


<pre><code>
int64() = -9223372036854775808..9223372036854775807
</code></pre>




### <a name="type-object">object()</a> ###


<pre><code>
object() = undefined | boolean() | <a href="#type-int64">int64()</a> | float() | <a href="calendar.md#type-datetime">calendar:datetime()</a> | {uid, <a href="#type-uint64">uint64()</a>} | [<a href="#type-object">object()</a>] | #{binary() =&gt; <a href="#type-object">object()</a>}
</code></pre>




### <a name="type-uint64">uint64()</a> ###


<pre><code>
uint64() = 0..18446744073709551615
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(Data) -&gt; {ok, DecodedObject} | {error, Error}
</code></pre>

<ul class="definitions"><li><code>Data = iodata()</code></li><li><code>DecodedObject = <a href="#type-object">object()</a></code></li><li><code>Error = {exception, atom(), term(), [term()]}</code></li></ul>

