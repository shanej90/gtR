# gtR 0.4.0

* Cleaned up `gtr_combinations` list.
* Tidied up `get_resources()` so one can return a single object within the chosen resource using its ID (eg, return a single organisation, a single project, etc).
* Added `get_links()` to return details of entities linked to the one(s) returned. This can be used 'downstream' to pick up other details.

# gtR 0.3.1

* Reduced sleep time between API calls from 5 to 1.5 seconds.
* Extended list of possible combinations.
* Corrected typo in collaboration endpoint.

# gtR 0.3.0

* Function that encompasses and replaces all previous query functions (`get_resources()`).

# gtR 0.2.0

* Functions to run queries against a combination of resources (`query_resource_combination()`), and a list of resource combinations (`gtr_combinations`).

# gtR 0.1.0

* Functions to get API configuration for each resource type (`get_configs()`), and generic function to run queries against each resource(`query_resource_all()`.
