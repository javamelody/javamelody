## Contributing

If you would like to contribute code, then you can create a pull request as you wish and we will fix the code afterward.

Or you may do so through the following canonical process:

* Fork the repository on GitHub and clone your fork.
* Configure the upstream remote, so that you can keep your fork synchronized.
```
git remote add upstream https://github.com/javamelody/javamelody.git
```
* Create a branch for your changes.
```
git branch my-awesome-bug-fix
git checkout my-awesome-bug-fix
```
* Check for and merge upstream changes.
```
git fetch upstream
git checkout master
git merge upstream/master
git checkout my-awesome-bug-fix
git rebase master
```
* Build your changes and run unit tests.  You will see many warnings and errors because unit tests validate error conditions.
As long as the final result is BUILD SUCCESS, then the tests have passed.
```
mvn -f javamelody-core test
...
BUILD SUCCESS
```
* Push your changes and send a pull request.
```
git push origin
```

When submitting code, please follow existing [conventions and style](../../wiki/DevGuide#development) in order to keep the code as readable as possible.

## License

By contributing your code, you agree to license your contribution under the terms of the [ASL](http://www.apache.org/licenses/LICENSE-2.0).

All files are released with the Apache 2.0 license.

If you are adding a new file it should have a header like this:

```
/*
 * Copyright 2008-2019 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
```
