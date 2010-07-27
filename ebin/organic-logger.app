{application, organic-logger,
 [{description, "Organic Logger"},
  {vsn, "1"},
  {modules, [organic-logger]},
  {registered, [organic.logger]},
  {applications, [kernel,stdlib,sasl]},
  {mod, {.organic.logger.app,[]}}
 ]}.
