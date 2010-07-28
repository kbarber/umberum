{application, 'organic.logger',
 [{description, "Organic Logger"},
  {vsn, "1"},
  {modules, [organic.logger.sup]},
  {registered, []},
  {applications, [kernel,stdlib,sasl]},
  {mod, {organic.logger.app,[]}}
 ]}.
