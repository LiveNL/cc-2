begin
    x := (a + b) * x;
    y := a * b;
    while (a * b > a + b) do {
      a := a + 1;
      x := a + b;
    }
end
