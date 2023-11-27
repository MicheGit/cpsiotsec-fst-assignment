{application, sr,
  [{description,  "State replication with digital twins"},
   {id,           "sr"},
   {vsn,          "0.0.0"},
   {modules,      [
    sr_sink, 
    sr_env, 
    sr_mitm, 
    sr_plc, 
    sr_pusher, 
    sr_sink, 
    sr_system, 
    sr_rfid_reader, 
    sr_malicious_plc,
    id_env
    ]},
    {mod, {id_env, []}}
  ]}.
