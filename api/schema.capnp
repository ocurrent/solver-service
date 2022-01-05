
@0x8f8a99339e493464;

interface Log {
  write @0 (msg :Text);
}

interface Solver {
  solve @0 (request :Text, log :Log) -> (response :Text);
}
