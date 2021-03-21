module JWrite = Scrutiny_systemd.Journal.Write

let () =
  JWrite.write ~tag:"a tag" ~level:Warning
    ~extra:[ ("CODE_LINE", string_of_int __LINE__); ("CODE_FILE", __FILE__) ]
    "Hello, world!"
