'use strict';

// module Swagger.TypeGen

var yaml = require('js-yaml');

exports.parseYaml = function(file) {
  return yaml.safeLoad(file);
}
