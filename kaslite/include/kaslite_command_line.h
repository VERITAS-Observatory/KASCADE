!KASLITE Command line options  
        character*80  input_par_file_name !-p
        character*80  random_seed_file_name  !-r
        character*80  segment_input_file !-i
        character*80  pe_output_file !-o
        character*80  benchmark_text !-o
        logical*1     benchmark_flag,make_pe_file
        common/cmdlineinput/input_par_file_name,random_seed_file_name,
     1         segment_input_file,pe_output_file,benchmark_text,
     1         benchmark_flag,make_pe_file
  
