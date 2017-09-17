# rssconcat
Service that allows to concatenate several RSS feeds into one. Multiple concatenated feeds can be served on different URLs.

## Little Q&A
* **Why?**  
I like to use RSS to get some kind of info that updates regulary. And sometimes it's usefull to have several similar feeds to be represented as one feed. I used several online services for these, but all of them have either some limitations or downtimes. So I decided to create simple application that provides such service and I don't need to rely on 3rd party services.
* **Why Haskell?**  
Why not? :)  
I don't use Haskell for my everyday tasks. In fact I never used it for anything except for examples from [Learn You a Haskell for Great Good](http://learnyouahaskell.com/). So I decided to check if I know it good enough to write some simple application. This one is a good fit. It's not too complex. And, at the same time, it includes such common things as network (web) server, client, file reading, markup language parsing.
* **Can I use it as example of ~~good~~ Haskell app?**  
You probably should not. As it is mentioned above, this is my first Haskell app, and I believe that it has a lot of things that can be improved. 


## Usage
* Download
* Build with stack
* Create config file. Use [config.example.yaml](./config.example.yaml) for reference.
* Run rssconcat from build directory (or `stack exec rssconcat`) with one optional argument - path to config file. If run without arguments, it looks for `config.yaml` file in current working directory.
* Go to http://YOUR_HOST:YOUR_PORT/ for list of availabled feed or add http://YOUR_HOST:YOUR_PORT/YOUR_FEED_NAME directly to your RSS reader.

## TODO
* Cover with tests.
* Handle incorrect config file.
* Handle errors when retrieving from source feeds.
* Support for `<link>` RSS chanel field as [required by specification](http://www.rssboard.org/rss-specification#requiredChannelElements)  
For example: add new URL like /feed_name/info, which contains simple text like:
  > This is feed that combines the following feeds: *list of links from source feeds `<link>` fields*
* Put better description in `<description>` RSS chanel field. Probably with the same content as described above.
* Support different feed types (RSS1/RSS2/Atom).
* ???
* PROFIT!

## Contributing
PRs are welcome. Also I would really appreciate, if your PR contains not only description of what is changed, but also why. E.g.: this PR changes usage of `&` function to `$` because former is considered confusing and against Haskell best practices. Such comments will help me to learn Haskell better.