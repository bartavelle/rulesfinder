my @list;
while(<>)
{
    s/\s*$//;
    next if (/:/);
    s/Al".*//;
    s/A0".*//;
    s/ NBPWD=.*//;
    s/\s*Q\s*$//;
    push @list, $_;
}

my $n = 0;
foreach my $rulea (@list)
{
    foreach my $ruleb (@list)
    {
        next if $rulea eq $ruleb;
        $n++;
        open(my $fd, ">rules/combined_results_$n.rule");
        print $fd "$rulea $ruleb Q\n";
        close($fd);
    }
}
