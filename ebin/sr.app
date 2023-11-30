{application, sr,
  [{description,  "State replication with digital twins"},
   {id,           "sr"},
   {vsn,          "0.0.0"},
   {modules,      [
    sr_twin,
    sr_mitm, 
    sr_plc, 
    sr_pusher, 
    sr_rfid_reader, 
    id_env
    ]},
    {mod, {id_env, []}}
  ]}.
