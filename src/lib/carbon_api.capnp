@0xb8d9dcb6ce8d71bf;

interface Reporter {
    report      @0 (machine :Text, msg :Text) -> (reply :Text);
}